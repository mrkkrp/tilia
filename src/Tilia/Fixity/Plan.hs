{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | "Tilia.Fixity" resolves a module's operators exactly, given a function
-- that says what each imported module exports. This is that function, built
-- from what the project itself is compiled against.
module Tilia.Fixity.Plan
  ( Route (..),
    Resolver (..),
    newResolver,
    newResolverVia,
    newResolverWith,
    withReexports,
    scopeFor,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.DeepSeq (force)
import Control.Exception (bracket_, evaluate)
import Control.Monad (filterM, join)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BL
import Data.Choice (Choice, fromBool, pattern Do)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Hs (HsModule)
import GHC.Hs.Extension (GhcPs)
import GHC.LanguageExtensions.Type (Extension (ImplicitPrelude))
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Tilia.Cabal.Package (PackageReader, declaredModules, newPackageReader, readPackageDescription)
import Tilia.Cabal.Plan
import Tilia.Cabal.Project (cabalFilesIn)
import Tilia.Cpp.Directives (branchLeaves, withoutRuledOut)
import Tilia.Cpp.Macros (Macros (..))
import Tilia.Fixity
import Tilia.Fixity.Archive (Archive (..), InArchive (..), moduleEndings, moduleIn, pathOfModule, readArchive, writtenForHsc)
import Tilia.Fixity.ByHand (hscFixities)
import Tilia.Fixity.Cabal (declaredExtensions, packageModules)
import Tilia.Fixity.Cache
import Tilia.Fixity.HiFile (primopFixities)
import Tilia.Fixity.Interface
import Tilia.Fixity.PackageDb
import Tilia.Memo (memoized, newFlights, newMemo)
import Tilia.Parser
import Tilia.Pragma (effectiveExtensions)
import Tilia.Utils (byteOrderMark, inParallel, quietly, readFileText, sha256Of)

----------------------------------------------------------------------------
-- Resolving

-- | Where a module's fixities can be read from.
data Route
  = -- | The compiled interface the package database points at. Cheap, and
    -- authoritative where it exists, since it is the compiler's own account
    -- of what it settled on.
    FromInterface
  | -- | The module's source, out of the package's tarball in Cabal's
    -- package cache. Slower, and the only route for a package that is
    -- planned but not built.
    FromSource
  deriving (Eq, Show)

-- | What can be asked about a module, once a plan says where to look.
newtype Resolver = Resolver
  { -- | What reading a module established about the names it exports.
    askModule :: Text -> IO Established
  }

-- | Build a new 'Resolver'.
--
-- Answers are remembered on disk between runs by "Tilia.Fixity.Cache", so a
-- package is decompressed and parsed once per machine rather than once per
-- file.
newResolver ::
  -- | The build plan to use.
  BuildPlan ->
  IO Resolver
newResolver plan = do
  packageReader <- newPackageReader (plannedCompiler plan)
  newResolverVia (Do #useCache) [FromInterface, FromSource] packageReader plan

-- | 'newResolver', restricted to the routes given.
newResolverVia ::
  -- | Whether to use the cache.
  Choice "useCache" ->
  -- | Which readings to try, in order.
  [Route] ->
  -- | What the package of each of the project's own modules puts in force.
  PackageReader ->
  -- | The build plan to use.
  BuildPlan ->
  IO Resolver
newResolverVia caching routes packageReader plan = do
  cache <- openCache caching =<< tokenForBuildPlan plan
  tarballs <-
    if FromSource `elem` routes
      then plannedTarballs cache plan
      else pure []
  installed <- getInstalledPackages cache
  newResolverWith routes cache installed tarballs packageReader plan

-- | 'newResolverVia', given what it would find out from the machine.
newResolverWith ::
  -- | Which readings to try, in order.
  [Route] ->
  -- | Where to remember answers between runs.
  Cache ->
  -- | Every package the compiler can see.
  [InstalledPackage] ->
  -- | Every planned package that might have a tarball, and where it would
  -- be.
  [(PlanPackage, FilePath)] ->
  -- | What the package of each of the project's own modules puts in force.
  PackageReader ->
  -- | The build plan to use.
  BuildPlan ->
  IO Resolver
newResolverWith routes cache installed tarballs packageReader plan = do
  index <- buildModuleIndex cache installed tarballs
  let interfaces = interfaceIndex installed
  local <- localModules plan
  flights <- newFlights
  answersRead <- newMemo
  extensionsRead <- newMemo
  archivesRead <- newMemo
  interfacesRead <- newMemo
  reading <- newQSem =<< getNumCapabilities
  let interfaceOf modName =
        memoized flights interfacesRead Nothing modName $
          case Map.lookup modName primopFixities of
            Just declared -> pure (Just (asInterface declared))
            Nothing -> case Map.lookup modName interfaces of
              Nothing -> pure Nothing
              Just (_, path) ->
                bracket_ (waitQSem reading) (signalQSem reading) $
                  readInterface modName path
  let workings =
        Workings
          { wkRoutes = routes,
            wkCache = cache,
            wkLocal = local,
            wkIndex = index,
            wkInterfaces = interfaces,
            wkInterfaceOf = interfaceOf,
            wkReach = reach,
            wkSummariesOf = summariesOf,
            wkModuleInArchive = moduleInArchive,
            wkGenerated = generatedModules plan,
            wkReexported =
              Map.fromList [pair | i <- installed, pair <- ipReexports i]
          }
      -- The stand-in is what 'withReexports' makes of a module it is in the
      -- middle of reading.
      reach visiting modName
        | modName `Set.member` visiting = pure mempty
        | otherwise =
            memoized flights answersRead mempty modName $
              resolveModule workings visiting modName
      summariesOf modName text =
        extensionsOf modName >>= \case
          Nothing -> pure Nothing
          Just extensions -> do
            let summarized =
                  evaluate . force $
                    configurationsOf (macrosOf plan) extensions modName text
                stamp =
                  digestOf . T.intercalate "\n" $
                    [T.pack (show extensions), macrosRead, text]
            case digestOf . T.pack <$> Map.lookup modName local of
              Nothing -> summarized
              Just key ->
                join
                  <$> recalled
                    (cachedSummaries cache key stamp)
                    (storeSummaries cache key stamp)
                    (Just <$> summarized)
      macrosRead = T.pack (show (macrosOf plan))
      extensionsOf modName
        | Just path <- Map.lookup modName local =
            either (const Nothing) Just <$> packageReader path
        | Just (package, tarball) <- Map.lookup modName index =
            memoized flights extensionsRead Nothing package $
              fmap declaredExtensions . (archiveCabal =<<) <$> archiveOf tarball
        | otherwise = pure Nothing
      archiveOf tarball =
        memoized flights archivesRead Nothing (T.pack tarball) (readArchive tarball)
      moduleInArchive tarball modName = (moduleIn modName =<<) <$> archiveOf tarball
  pure Resolver{askModule = reach Set.empty}

-- | Work out what a module can see, using a resolver to reach its imports.
--
-- This is the join between the pure half of "Tilia.Fixity" and the half
-- that touches the disk: the imports are resolved first, and the scope is
-- then computed from the answers. Note that a name an import leaves
-- unsettled stays unsettled, which is what lets 'Tilia.Fixity.lookupFixity'
-- distinguish a conclusion from a guess.
scopeFor ::
  -- | What can be asked about the modules it imports.
  Resolver ->
  -- | Whether @ImplicitPrelude@ is on, which the module's own pragmas
  -- and its package's @default-extensions@ decide between them.
  Choice "implicitPrelude" ->
  -- | The configurations of the module whose scope is wanted, already
  -- parsed.
  NonEmpty (HsModule GhcPs) ->
  -- | Everything that module can see, and what it could not find out.
  IO Scope
scopeFor resolver implicitPrelude configurations = do
  let imports = moduleImports implicitPrelude configurations
  answers <-
    inParallel
      (\m -> (m,) <$> askModule resolver m)
      (fmap importModule imports)
  let table = Map.fromList answers
  pure $
    resolveScope
      implicitPrelude
      (\m -> Map.findWithDefault unreadable m table)
      configurations

-- | Everything a resolver consults, and the way back into it.
--
-- None of it changes from one module to the next, which is why
-- 'newResolverWith' builds it once and hands it over whole. What comes back
-- out of that is the 'Resolver'; this is what is behind it.
data Workings = Workings
  { -- | Which readings to try, in the order given.
    wkRoutes :: [Route],
    -- | Where to remember answers between runs.
    wkCache :: Cache,
    -- | The modules of the project's own packages, which are read straight
    -- from disk rather than out of an archive.
    wkLocal :: Map Text FilePath,
    -- | Which package holds each module, and the tarball to find it in; the
    -- package is the cache key, which carries the hash the tarball was
    -- verified against.
    wkIndex :: Map Text (Text, FilePath),
    -- | What to file an answer read out of each module's interface under.
    wkInterfaces :: Map Text (Text, FilePath),
    -- | A module's interface, read at most once a run.
    wkInterfaceOf :: Text -> IO (Maybe Interface),
    -- | How to reach another module. Tied back on itself by
    -- 'newResolverWith', so that the memo it keeps covers the recursive
    -- calls too.
    wkReach :: Set Text -> Text -> IO Established,
    -- | What each configuration of a module says, given its text, worked
    -- out once a run.
    wkSummariesOf :: Text -> Text -> IO (Maybe (NonEmpty ModuleSummary)),
    -- | What a tarball holds for a module, each tarball read once a run.
    wkModuleInArchive :: FilePath -> Text -> IO (Maybe InArchive),
    -- | The modules @cabal@ writes itself, which are therefore in no
    -- package's sources. See 'generatedModules'.
    wkGenerated :: Set Text,
    -- | The modules an installed package exposes that another one holds,
    -- each with its name there.
    wkReexported :: Map Text Text
  }

-- | What reading a module establishes, by the cheapest route that settles
-- it.
resolveModule ::
  -- | Where to look, and how to get back to the resolver.
  Workings ->
  -- | Modules currently being resolved further up the call chain.
  Set Text ->
  -- | The module to resolve.
  Text ->
  IO Established
resolveModule
  Workings
    { wkRoutes,
      wkCache,
      wkLocal,
      wkIndex,
      wkInterfaces,
      wkInterfaceOf,
      wkReach,
      wkSummariesOf,
      wkModuleInArchive,
      wkGenerated,
      wkReexported
    }
  visiting
  modName
    | Just path <- Map.lookup modName wkLocal,
      writtenForHsc path =
        pure (hscDeclares modName)
    | Just path <- Map.lookup modName wkLocal =
        readFileText path >>= \case
          Nothing -> pure unreadable
          Just source ->
            fromSummaries (wkReach visiting') modName
              =<< wkSummariesOf modName (snd (byteOrderMark source))
    | Just original <- Map.lookup modName wkReexported,
      Map.notMember modName wkInterfaces =
        wkReach visiting' original
    | otherwise = answered <$> firstAnswer (fmap taking wkRoutes)
    where
      visiting' = Set.insert modName visiting

      taking = \case
        FromInterface -> viaInterface
        FromSource -> viaArchive

      -- A route that read some of the module is kept in case no later one
      -- reads all of it.
      firstAnswer = go Nothing
        where
          go partial [] = pure (fromMaybe unreadable partial)
          go partial (route : rest) =
            route >>= \case
              Just established
                | settlesEverything established -> pure established
                | established /= unreadable -> go (partial <|> Just established) rest
              _ -> go partial rest

      viaInterface = case Map.lookup modName wkInterfaces of
        Nothing -> pure Nothing
        Just (key, _) ->
          throughCache key (Just <$> fromInterface wkInterfaceOf modName)

      viaArchive = case Map.lookup modName wkIndex of
        Nothing -> pure Nothing
        Just (package, tarball) ->
          throughCache package $
            fromSource
              wkModuleInArchive
              wkSummariesOf
              (wkReach visiting')
              tarball
              modName

      answered established
        | settlesEverything established = established
        | Set.member modName wkGenerated =
            established
              { establishedFixities = Map.empty,
                establishedUnsettled = Map.empty,
                establishedUntold = Set.empty
              }
        | otherwise = established
      throughCache key =
        recalled
          (cachedEstablished wkCache key modName)
          (storeEstablished wkCache key modName)

-- | Which package and tarball holds each module.
buildModuleIndex ::
  -- | Where to remember each package's module list.
  Cache ->
  -- | What the compiler says is installed. Empty if @ghc-pkg@ could not be
  -- run, in which case every package falls back to its @.cabal@ file.
  [InstalledPackage] ->
  -- | Every package that might have a tarball, and where it would be.
  [(PlanPackage, FilePath)] ->
  -- | For each module, the package that holds it (as a cache key) and
  -- the tarball holding its source.
  IO (Map Text (Text, FilePath))
buildModuleIndex cache installed tarballs =
  Map.fromListWith (\_ first' -> first') . concat <$> traverse one tarballs
  where
    byNameVersion =
      Map.fromList [((ipName i, ipVersion i), ipModules i) | i <- installed]
    one (p, tarball) = do
      let key = cacheKey p
      let exposed = Map.lookup (ppName p, ppVersion p) byNameVersion
      held <- fromCabalFile cache key tarball p
      let modules = case (exposed, held) of
            (Nothing, Nothing) -> []
            (a, b) -> concat (catMaybes [a, b])
      pure [(m, (key, tarball)) | m <- modules]

-- | Where each installed module's compiled interface is.
interfaceIndex :: [InstalledPackage] -> Map Text (Text, FilePath)
interfaceIndex installed =
  Map.fromListWith
    (\_ first' -> first')
    [ (m, (key, dir </> pathOfModule m <> ".hi"))
    | i <- installed,
      dir <- ipImportDirs i,
      -- Bound out here so that the directory is hashed once rather than
      -- once for each of the modules found in it.
      let key = keyFor dir,
      m <- ipModules i
    ]
  where
    keyFor dir = "interface-" <> digestOf (T.pack dir)

-- | A name for some text, short enough to file something under.
digestOf :: Text -> Text
digestOf = T.take 24 . sha256Of

-- | Present a fixity map as an 'Interface'.
asInterface :: Fixities -> Interface
asInterface fixities =
  Interface
    { interfaceDeclares = fixities,
      interfaceReexports = [],
      interfaceMembers = Map.empty,
      interfaceExports = Nothing
    }

-- | What a compiled interface says a module exports, with the fixities of
-- what it passes on read from where each is declared.
fromInterface ::
  -- | A module's interface, if it has one.
  (Text -> IO (Maybe Interface)) ->
  -- | The module to read.
  Text ->
  IO Established
fromInterface interfaceOf modName =
  interfaceOf modName >>= \case
    Nothing -> pure unreadable
    Just iface -> do
      declarers <-
        inParallel
          asked
          (distinct (fmap fst (interfaceReexports iface)))
      let declarer m = join (lookup m declarers)
      pure
        Established
          { establishedFixities =
              Map.union (interfaceDeclares iface) . Map.unions $
                [ Map.filterWithKey
                    (\(_, o) _ -> o == op)
                    (interfaceDeclares declared)
                | (m, op) <- interfaceReexports iface,
                  Just declared <- [declarer m]
                ],
            establishedUnsettled =
              Map.fromListWith
                Set.union
                [ ([m], Set.fromList [(InTypes, op), (InTerms, op)])
                | (m, op) <- interfaceReexports iface,
                  Nothing <- [declarer m]
                ],
            establishedUntold = Set.empty,
            establishedCertain = fromMaybe mempty (interfaceExports iface),
            establishedMembers = interfaceMembers iface
          }
  where
    asked m = do
      interface <- interfaceOf m
      pure (m, interface)
    distinct = Map.keys . Map.fromList . fmap (,())

-- | A package's module list from the @.cabal@ file in its tarball.
fromCabalFile ::
  -- | Where to remember the answer.
  Cache ->
  -- | What to file it under. Carries the hash the tarball was verified
  -- against, so a changed tarball misses rather than matching stale data.
  Text ->
  -- | The tarball to read the @.cabal@ file out of.
  FilePath ->
  -- | The package it belongs to, consulted for the hash to verify against.
  PlanPackage ->
  -- | The modules it holds, or 'Nothing' if the tarball is absent, fails
  -- verification, or holds no @.cabal@ file.
  IO (Maybe [Text])
fromCabalFile cache key tarball p =
  recalled (cachedModules cache key) (storeModules cache key) $
    verified p tarball >>= \case
      False -> pure Nothing
      True -> packageModules tarball

-- | Does the tarball hash to what the plan says it should?
verified :: PlanPackage -> FilePath -> IO Bool
verified p tarball = case sourceHashOf p of
  Nothing -> pure True
  Just expected ->
    quietly False $ do
      actual <- sha256OfFile tarball
      pure (actual == T.toLower expected)

-- | The SHA-256 of a file, as lower-case hex.
sha256OfFile :: FilePath -> IO Text
sha256OfFile path = do
  bytes <- BL.readFile path
  pure (T.decodeUtf8Lenient (B16.encode (SHA256.hashlazy bytes)))

-- | Read what a module exports out of a tarball, following re-exports.
fromSource ::
  -- | What a tarball holds for a module.
  (FilePath -> Text -> IO (Maybe InArchive)) ->
  -- | What each configuration of a module says, given its text.
  (Text -> Text -> IO (Maybe (NonEmpty ModuleSummary))) ->
  -- | How to reach another module, for chasing re-exports. This is
  -- 'resolveModule' tied back on itself, with the visiting set already
  -- extended.
  (Text -> IO Established) ->
  -- | The tarball holding this module's source.
  FilePath ->
  -- | The module to read.
  Text ->
  -- | What reading it established, or 'Nothing' if there is no archive to
  -- read it from.
  IO (Maybe Established)
fromSource moduleInArchive summariesOf reach tarball modName =
  doesFileExist tarball >>= \case
    False -> pure Nothing
    True ->
      fmap Just $
        moduleInArchive tarball modName >>= \case
          Nothing -> pure unreadable
          Just ForHsc -> pure (hscDeclares modName)
          Just (Haskell source) ->
            fromSummaries reach modName =<< summariesOf modName source

-- | What a module exports, out of what each of its configurations says.
fromSummaries ::
  -- | How to reach another module, for chasing re-exports. This is
  -- 'resolveModule' tied back on itself, with the visiting set already
  -- extended.
  (Text -> IO Established) ->
  -- | The name the module was looked up under.
  Text ->
  -- | What each configuration of it says, or 'Nothing' where none of them
  -- parses.
  Maybe (NonEmpty ModuleSummary) ->
  IO Established
fromSummaries reach modName = \case
  Nothing -> pure unreadable
  Just summaries ->
    agreeing <$> traverse (withReexports reach modName) summaries

-- | One answer from every configuration of a module that counts.
--
-- A module may declare a fixity in one configuration and a different one in
-- another. Which of them holds depends on how the module is compiled, which
-- is not ours to decide, so disagreement leaves the name unsettled.
-- Agreement across the ones that count is an answer, and a stronger one
-- than the blanked text could give.
--
-- A configuration that leaves names unsettled is passed over where another
-- settles every name, because almost every one of those is a branch meant
-- for a different platform. Where none does, every configuration counts,
-- and a name any of them leaves unsettled stays unsettled.
agreeing :: NonEmpty Established -> Established
agreeing answers =
  settledOnly
    Established
      { establishedFixities = Map.mapMaybe sole declared,
        establishedUnsettled =
          Map.unionsWith
            Set.union
            ( maybe
                Map.empty
                (Map.singleton [])
                disagreed
                : fmap establishedUnsettled (toList counted)
            ),
        establishedUntold = foldMap establishedUntold counted,
        establishedCertain = foldr1 inEvery (fmap establishedCertain counted),
        establishedMembers =
          Map.unionsWith Set.union (fmap establishedMembers counted)
      }
  where
    counted =
      fromMaybe answers $
        NE.nonEmpty (NE.filter settlesEverything answers)
    declared =
      Map.unionsWith
        (<>)
        [Map.map pure (establishedFixities a) | a <- toList counted]
    sole (fixity :| rest) =
      if all (== fixity) rest then Just fixity else Nothing
    disagreed = inhabited (Map.keysSet (Map.filter (isNothing . sole) declared))
    inEvery a b =
      Certain
        { certainNames = Set.intersection (certainNames a) (certainNames b),
          certainMembers =
            Map.intersectionWith
              Set.intersection
              (certainMembers a)
              (certainMembers b)
        }

-- | Where each module that lives in a directory rather than an archive is.
localModules :: BuildPlan -> IO (Map Text FilePath)
localModules plan =
  Map.unions <$> traverse forPackage (concatMap directoryOf (bpPackages plan))
  where
    directoryOf p = case ppSource p of
      LocalPackage dir -> [dir]
      CheckedOut dir -> [dir]
      _ -> []
    forPackage dir = quietly Map.empty $ do
      cabalFilesIn dir >>= \case
        [] -> pure Map.empty
        (cabalFile : _) ->
          readPackageDescription cabalFile >>= \case
            Left _ -> pure Map.empty
            Right described ->
              Map.fromListWith (\_ first' -> first') . concat
                <$> traverse (locate dir) (declaredModules described)
    locate dir (m, unwritten) = do
      found <- filterM doesFileExist [dir </> unwritten <> e | e <- moduleEndings]
      pure [(m, path) | path <- take 1 found]

-- | What a module exports, out of what it declares and what its imports
-- bring in.
withReexports ::
  -- | How to reach another module, for names this one only passes on.
  (Text -> IO Established) ->
  -- | The name this module was looked up under.
  Text ->
  -- | The module.
  ModuleSummary ->
  IO Established
withReexports reach modName summary =
  case summaryExports summary of
    Nothing -> pure own
    Just items -> do
      listed <-
        reached $ \i ->
          any (suppliedBy i) (concatMap named items)
            || ( not (importQualified i)
                   && importAlias i
                     `elem` [m | ExportModule m <- items]
               )
      let reexported =
            Map.fromList
              [ ((qualifier, parent), membersFrom listed qualifier parent)
              | ExportAll qualifier parent <- items,
                Map.notMember parent declared
              ]
          members =
            [ (qualifier, kid)
            | ((qualifier, _), Just kids) <- Map.toList reexported,
              kid <- Set.toList kids,
              Set.notMember kid defined
            ]
      more <- reached $ \i ->
        Map.notMember (importModule i) listed && any (suppliedBy i) members
      let answers = Map.union listed more
          parts = fmap (exported answers reexported) items
      pure . together $
        mempty
          { establishedFixities =
              Map.restrictKeys
                (summaryFixities summary)
                (foldMap (certainNames . establishedCertain) parts),
            establishedMembers = summaryListedMembers summary
          }
          : parts
  where
    imports = summaryImports summary
    declared = summaryDeclaredMembers summary
    defined =
      Set.map snd (summaryNames summary)
        <> Set.map snd (Map.keysSet (summaryFixities summary))
    own =
      mempty
        { establishedFixities = summaryFixities summary,
          establishedCertain = Certain (summaryNames summary) declared,
          establishedMembers = Map.map (Set.map snd) declared
        }
    reached wanted =
      Map.fromList
        <$> traverse
          (\m -> (m,) <$> reach m)
          (Set.toList (Set.fromList [importModule i | i <- imports, wanted i]))
    suppliedBy i (qualifier, op) = maySupply Map.empty qualifier op i
    named = \case
      ExportName _ qualifier op ->
        [(qualifier, op) | Set.notMember op defined]
      ExportAll qualifier parent ->
        [(qualifier, parent) | Set.notMember parent defined]
      ExportSome qualifier parent kids ->
        [(qualifier, op) | op <- parent : kids, Set.notMember op defined]
      ExportModule _ -> []

    exported answers reexported = \case
      ExportName namespace qualifier op ->
        settled answers True qualifier (namespace, op)
          <> certainly (Set.singleton (namespace, op)) Map.empty
      ExportAll qualifier parent
        | Just kids <- Map.lookup parent declared -> withMembers parent kids
        | otherwise ->
            let found = settled answers True qualifier (InTypes, parent)
                kids =
                  Map.findWithDefault Nothing (qualifier, parent) reexported
             in found
                  <> mempty{establishedUntold = Map.keysSet (establishedUnsettled found)}
                  <> foldMap (member answers qualifier parent) (foldMap Set.toList kids)
                  <> case kids of
                    Nothing -> certainly (Set.singleton (InTypes, parent)) Map.empty
                    Just known ->
                      withMembers parent (certainMembersOf answers qualifier parent)
                        <> mempty{establishedMembers = Map.singleton parent known}
      ExportSome qualifier parent kids ->
        settled answers True qualifier (InTypes, parent)
          <> foldMap (member answers qualifier parent) kids
          <> withMembers
            parent
            ( Set.filter
                ((`elem` kids) . snd)
                ( fromMaybe
                    (certainMembersOf answers qualifier parent)
                    (Map.lookup parent declared)
                )
            )
      ExportModule m ->
        (if Just m == summaryName summary || m == modName then own else mempty)
          <> foldMap
            (whole answers)
            [i | i <- imports, not (importQualified i), importAlias i == m]

    certainly names members = mempty{establishedCertain = Certain names members}
    withMembers parent kids =
      certainly (Set.insert (InTypes, parent) kids) (Map.singleton parent kids)
    member answers qualifier parent kid
      | Set.member kid defined = mempty
      | otherwise =
          case [ (i, established, certainKids)
               | (i, established) <- candidates answers qualifier parent,
                 decides established (InTypes, parent) i,
                 let certainKids =
                       Set.filter
                         ((== kid) . snd)
                         ( Map.findWithDefault
                             Set.empty
                             parent
                             (certainMembers (establishedCertain established))
                         ),
                 not (Set.null certainKids)
               ] of
            (i, established, certainKids) : _ ->
              foldMap (from [(i, established)]) certainKids
            [] ->
              foldMap
                ( \namespace ->
                    settled answers False qualifier (namespace, kid)
                )
                [InTypes, InTerms]

    candidates answers qualifier op =
      [ (i, established)
      | i <- imports,
        Just established <- [Map.lookup (importModule i) answers],
        maySupply (establishedMembers established) qualifier op i
      ]

    settled answers certain qualifier name@(_, op)
      | Set.member op defined = mempty
      | otherwise = case [ (i, established)
                         | certain,
                           (i, established) <- found,
                           decides established name i
                         ] of
          decider : _ -> from [decider] name
          [] -> from found name
      where
        found = candidates answers qualifier op

    from imported name =
      case [ importModule i : chain
           | (i, established) <- imported,
             chain <- unsettledThrough established name
           ] of
        [] ->
          mempty
            { establishedFixities =
                Map.fromList
                  ( take
                      1
                      [ (name, fixity)
                      | (_, a) <- imported,
                        Just fixity <- [Map.lookup name (establishedFixities a)]
                      ]
                  )
            }
        chains ->
          mempty
            { establishedUnsettled =
                Map.fromList [(chain, Set.singleton name) | chain <- chains]
            }

    membersFrom answers qualifier parent =
      case mapMaybe
        (Map.lookup parent . establishedMembers . snd)
        (candidates answers qualifier parent) of
        [] -> Nothing
        kids -> Just (Set.unions kids)

    certainMembersOf answers qualifier parent =
      Set.unions
        [ Set.filter (\kid -> certainlyBrings (establishedMembers established) kid i) kids
        | (i, established) <- candidates answers qualifier parent,
          let certain = establishedCertain established,
          Set.member (InTypes, parent) (certainNames certain),
          Just kids <- [Map.lookup parent (certainMembers certain)]
        ]

    whole answers i =
      Established
        { establishedFixities =
            Map.filterWithKey
              (\(_, op) _ -> admitted op)
              (establishedFixities established),
          establishedUnsettled =
            Map.mapKeys
              (importModule i :)
              ( Map.mapMaybe
                  (inhabited . Set.filter (admitted . snd))
                  (establishedUnsettled established)
              ),
          establishedUntold =
            Set.map
              (importModule i :)
              (establishedUntold established),
          establishedCertain =
            Certain
              { certainNames = Set.filter included (certainNames certain),
                certainMembers =
                  Map.map
                    (Set.filter included)
                    ( Map.filterWithKey
                        (\parent _ -> included (InTypes, parent))
                        (certainMembers certain)
                    )
              },
          establishedMembers = establishedMembers established
        }
      where
        established = Map.findWithDefault mempty (importModule i) answers
        certain = establishedCertain established
        admitted op = maySupply (establishedMembers established) Nothing op i
        included name = certainlyBrings (establishedMembers established) name i

-- | The parts of what a module exports, together.
--
-- A name one part certainly exports with a settled fixity is the one every
-- other part exports under that name too, or the module would not compile,
-- so no part leaves it unsettled.
together :: [Established] -> Established
together parts =
  settledOnly
    combined
      { establishedUnsettled =
          Map.mapMaybe
            (inhabited . (`Set.difference` certain))
            (establishedUnsettled combined)
      }
  where
    combined = mconcat parts
    certain =
      Set.unions
        [ Set.filter
            (null . unsettledThrough part)
            (certainNames (establishedCertain part))
        | part <- parts
        ]

-- | An answer without the fixities of the names it leaves unsettled.
settledOnly :: Established -> Established
settledOnly established
  | settlesEverything established = established
  | otherwise =
      established
        { establishedFixities =
            Map.filterWithKey
              (\name _ -> null (unsettledThrough established name))
              (establishedFixities established)
        }

-- | A set, unless it is empty.
inhabited :: Set a -> Maybe (Set a)
inhabited names
  | Set.null names = Nothing
  | otherwise = Just names

-- | Every configuration the preprocessor allows of a module's text that is
-- Haskell, parsed and summarized.
configurationsOf ::
  -- | What the plan settles about the questions its conditionals ask.
  Macros ->
  -- | What extensions the module's package puts in force.
  [Extension] ->
  -- | The module's name, for the parser to put in its errors.
  Text ->
  -- | Its text.
  Text ->
  Maybe (NonEmpty ModuleSummary)
configurationsOf macros extensions modName text =
  NE.nonEmpty . mapMaybe parsed
    =<< whatParsed (branchLeaves (withoutRuledOut macros text))
  where
    parsed leaf =
      summarize (hasImplicitPrelude extensions leaf) . pmModule
        <$> whatParsed (parseModule (ParserConfig extensions) named leaf)
    whatParsed = either (const Nothing) Just
    named = T.unpack modName

-- | Does this module see the Prelude without importing it?
hasImplicitPrelude :: [Extension] -> Text -> Choice "implicitPrelude"
hasImplicitPrelude extensions source =
  fromBool (ImplicitPrelude `elem` effectiveExtensions extensions source)

-- | What an @.hsc@ module declares, which is nothing unless it is named.
hscDeclares :: Text -> Established
hscDeclares modName =
  mempty
    { establishedFixities =
        maybe Map.empty inBothNamespaces (Map.lookup modName hscFixities)
    }
