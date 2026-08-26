{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Layer 3: finding out what the modules a project imports actually
-- declare.
--
-- "Tilia.Fixity" resolves a module's operators exactly, given a function
-- that says what each imported module exports. This is that function, built
-- from what the project itself is compiled against.
--
-- == How
--
-- @cabal@ writes the resolved build plan to @dist-newstyle\/cache\/plan.json@:
-- every package, at the exact version the project builds with. Each entry is
-- one of two kinds, and the distinction turns out to be exactly the one that
-- matters here.
--
--   * @configured@ packages come from Hackage, and @cabal@ keeps their source
--     tarballs in its package cache. Their fixity declarations can be read
--     out of the source.
--
--   * @pre-existing@ packages ship with the compiler—@base@, @ghc-prim@,
--     @containers@ and the rest. Their sources are not in the cache, so
--     their fixities come from 'builtinFixities'.
--
-- That split is not an approximation creeping back in. The boot packages are
-- a fixed, small set whose operators are stable across releases, which is
-- what makes a table of them reasonable where a table of Hackage would not
-- be.
--
-- == Finding the module
--
-- Which package exposes a module is answered by "Tilia.Fixity.Cabal",
-- reading @exposed-modules@ out of each planned package's own @.cabal@
-- file. Nothing has to be installed for that, only downloaded, which is
-- what 'checkReadiness' and 'prepare' are for.
module Tilia.Fixity.Plan
  ( -- * Build plans
    PlanPackage (..),
    PackageSource (..),
    isPreExisting,
    isFetchable,
    sourceHashOf,
    BuildPlan (..),
    readBuildPlan,

    -- * Readiness
    Readiness (..),
    planPathFor,
    checkReadiness,
    prepare,
    loadPlan,

    -- * Resolving
    newResolver,
    scopeFor,
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Monad (filterM, join)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.:), (.:?))
import Data.ByteString.Base16 qualified as B16
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.List (isSuffixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Hs (HsModule)
import GHC.Hs.Extension (GhcPs)
import System.Directory
  ( doesFileExist,
    getHomeDirectory,
    getModificationTime,
    listDirectory,
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readCreateProcessWithExitCode, proc, cwd)
import Tilia.Cpp (blankCpp)
import Tilia.Fixity
import Tilia.Fixity.Builtin (builtinFixities)
import Tilia.Fixity.Cabal (exposedModules, packageModules, sourceDirs)
import Tilia.Fixity.Cache
import Tilia.Fixity.PackageDb
import Tilia.Parser
import Tilia.Utils (quietly)

----------------------------------------------------------------------------
-- The plan

-- | Where a package's source is, if anywhere.
--
-- A plan contains exactly three kinds of entry and they are mutually
-- exclusive, which two independent flags could not say: a package cannot be
-- both shipped with the compiler and fetched from Hackage. Each carries
-- what is peculiar to it and nothing else, so there is no hash to consult
-- on a package that has no tarball, and no tarball to look for on one that
-- is a directory.
data PackageSource
  = -- | Already installed, so @cabal@ will not build it.
    --
    -- Not the same as "ships with the compiler", though it includes those.
    PreExisting
  | -- | A directory on this machine—the project being formatted, or a
    -- sibling of it in the same repository.
    LocalPackage FilePath
  | -- | Fetched from Hackage as a tarball, with the SHA-256 the plan
    -- expects it to have.
    --
    -- The hash is optional only because a plan is not obliged to record
    -- one; every entry in a plan @cabal@ writes for a secure repository
    -- does.
    HackagePackage (Maybe Text)
  deriving (Eq, Show)

-- | One package of a build plan.
data PlanPackage = PlanPackage
  { ppName :: Text,
    ppVersion :: Text,
    ppSource :: PackageSource
  }
  deriving (Eq, Show)

-- | Will @cabal@ decline to build this package because it is there already?
isPreExisting :: PlanPackage -> Bool
isPreExisting p = ppSource p == PreExisting

-- | Is there a tarball to go and read?
isFetchable :: PlanPackage -> Bool
isFetchable p = case ppSource p of
  HackagePackage _ -> True
  _ -> False

-- | The SHA-256 the plan expects this package's tarball to have.
sourceHashOf :: PlanPackage -> Maybe Text
sourceHashOf p = case ppSource p of
  HackagePackage hash -> hash
  _ -> Nothing

-- | A resolved build plan.
data BuildPlan = BuildPlan
  { bpCompiler :: Text,
    bpPackages :: [PlanPackage]
  }
  deriving (Eq, Show)

instance FromJSON BuildPlan where
  parseJSON = withObject "BuildPlan" $ \o ->
    BuildPlan
      <$> o .: "compiler-id"
      <*> o .: "install-plan"

instance FromJSON PlanPackage where
  parseJSON = withObject "PlanPackage" $ \o -> do
    name <- o .: "pkg-name"
    version <- o .: "pkg-version"
    kind <- o .:? "type"
    sourceKind <- o .:? "pkg-src" >>= traverse (.: "type")
    sourcePath <- o .:? "pkg-src" >>= traverse (.:? "path")
    sourceHash <- o .:? "pkg-src-sha256"
    pure
      PlanPackage
        { ppName = name,
          ppVersion = version,
          ppSource = case (kind :: Maybe Text, sourceKind :: Maybe Text) of
            (Just "pre-existing", _) -> PreExisting
            (_, Just "repo-tar") -> HackagePackage sourceHash
            (_, Just "local") -> LocalPackage (maybe "" T.unpack (join sourcePath))
            -- Anything else is treated as already present.
            _ -> PreExisting
        }

-- | Read @plan.json@.
readBuildPlan :: FilePath -> IO (Either Text BuildPlan)
readBuildPlan path =
  doesFileExist path >>= \case
    False -> pure (Left ("no build plan at " <> T.pack path))
    True -> either (Left . T.pack) Right <$> eitherDecodeFileStrict path

----------------------------------------------------------------------------
-- Resolving

-- | Build a lookup function for "Tilia.Fixity".
--
-- 'Nothing' means the module could not be read, which is not the same as
-- its having no operators; see 'Tilia.Fixity.resolveScope' for why the
-- difference has to survive.
--
-- Answers are remembered on disk between runs by "Tilia.Fixity.Cache", so
-- a package is decompressed and parsed once per machine rather than once
-- per file.
newResolver :: BuildPlan -> IO (Text -> IO (Maybe (Map OpName Fixity)))
newResolver plan = do
  tarballs <- plannedTarballs plan
  cache <- openCache
  installed <- readInstalledPackages
  index <- buildModuleIndex cache installed tarballs
  local <- localModules plan
  memo <- newIORef Map.empty
  let reach visiting modName = do
        known <- readIORef memo
        case Map.lookup modName known of
          Just answer -> pure answer
          Nothing -> do
            answer <- resolveModule cache local index reach visiting modName
            modifyIORef' memo (Map.insert modName answer)
            pure answer
  pure (reach Set.empty)

-- | Work out what a module can see, using a resolver to reach its imports.
--
-- This is the join between the pure half of "Tilia.Fixity" and the half
-- that touches the disk: the imports are resolved first, and the scope is
-- then computed from the answers. Note that an import the resolver could
-- not read arrives as 'Nothing' and stays 'Nothing', which is what lets
-- 'Tilia.Fixity.lookupFixity' distinguish a conclusion from a guess.
scopeFor ::
  -- | What each imported module exports, or 'Nothing' where that could not
  -- be determined
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  -- | The module whose scope is wanted, already parsed
  HsModule GhcPs ->
  -- | Everything that module can see, and what it could not find out
  IO Scope
scopeFor resolve hsModule = do
  let imported = map importModule (moduleImports hsModule)
  answers <- traverse (\m -> (m,) <$> resolve m) imported
  let table = Map.fromList answers
  pure (resolveScope (\m -> Map.findWithDefault Nothing m table) hsModule)

-- | Where a module's fixities come from, in order of cost.
resolveModule ::
  -- | Where to remember answers between runs.
  Maybe Cache ->
  -- | The modules of the project's own packages, which are read straight
  -- from disk rather than out of an archive.
  Map Text FilePath ->
  -- | Which package holds each module, and the tarball to find it in; the
  -- package is the cache key, which carries the hash the tarball was
  -- verified against.
  Map Text (Text, FilePath) ->
  -- | How to reach another module. Tied back on itself by 'newResolver', so
  -- that the memo it keeps covers the recursive calls too.
  (Set Text -> Text -> IO (Maybe (Map OpName Fixity))) ->
  -- | Modules currently being resolved further up the call chain.
  --
  -- A module reached while it is in here is part of a cycle, and yields
  -- nothing rather than recurring forever. Because that answer depends on
  -- how the module was reached, it is never remembered.
  Set Text ->
  -- | The module to resolve.
  Text ->
  -- | Its operator fixities, or 'Nothing' if they could not be
  -- established.
  IO (Maybe (Map OpName Fixity))
resolveModule cache local index reach visiting modName
  | Just builtin <- Map.lookup modName builtinFixities = pure (Just builtin)
  | modName `Set.member` visiting = pure (Just Map.empty)
  | Set.size visiting > reexportDepth = pure (Just Map.empty)
  -- The project's own modules come first and are never remembered on disk:
  -- they are the ones being edited, so an answer kept from a previous run is
  -- the one thing here that can be out of date.
  | Just path <- Map.lookup modName local =
      readFileText path >>= \case
        Nothing -> pure Nothing
        Just source -> fromText (reach visiting') visiting' source modName
  | otherwise = case Map.lookup modName index of
      Nothing -> pure Nothing
      Just (package, tarball) ->
        cachedFor package >>= \case
          Just remembered -> pure (Just remembered)
          Nothing ->
            fromSource (reach visiting') visiting' tarball modName >>= \case
              Nothing -> pure Nothing
              Just fixities -> do
                storeFor package fixities
                pure (Just fixities)
  where
    visiting' = Set.insert modName visiting
    cachedFor package = case cache of
      Nothing -> pure Nothing
      Just c -> cachedFixities c package modName
    storeFor package fixities = case cache of
      Nothing -> pure ()
      Just c -> storeFixities c package modName fixities

-- | Which package and tarball holds each module.
--
-- The module list of a package is itself cached: it comes from a @.cabal@
-- file inside an archive, and reading seventy of those is the bulk of what
-- starting up costs.
--
-- Where two packages expose the same module the first is kept. A plan that
-- builds cannot contain such a pair for any module the project imports, so
-- the choice only ever falls on a module nothing will ask about.
buildModuleIndex ::
  -- | Where to remember each package's module list, if anywhere.
  Maybe Cache ->
  -- | What the compiler says is installed. Empty if @ghc-pkg@ could not be
  -- run, in which case every package falls back to its @.cabal@ file.
  [InstalledPackage] ->
  -- | Every package that might have a tarball, and where it would be.
  [(PlanPackage, FilePath)] ->
  -- | For each module, the package that exposes it (as a cache key) and
  -- the tarball holding its source.
  IO (Map Text (Text, FilePath))
buildModuleIndex cache installed tarballs =
  Map.fromListWith (\_ first' -> first') . concat <$> traverse one tarballs
  where
    -- What the compiler reports, by name and version, so that a package can
    -- be looked up without trusting how the plan classified it.
    byNameVersion =
      Map.fromList [((ipName i, ipVersion i), ipModules i) | i <- installed]

    one (p, tarball) = do
      let key = cacheKey p
      modules <- case Map.lookup (ppName p, ppVersion p) byNameVersion of
        -- The compiler knows. This is the fast path and the only one that
        -- works where the plan calls everything pre-existing.
        Just ms -> pure (Just ms)
        -- It does not, so read the package's own @.cabal@ out of its
        -- tarball. This covers a dependency that has been downloaded but
        -- not yet built.
        Nothing -> fromCabalFile cache key tarball p
      pure [(m, (key, tarball)) | m <- concat modules]

-- | A package's module list from the @.cabal@ file in its tarball.
fromCabalFile ::
  -- | Where to remember the answer, if anywhere.
  Maybe Cache ->
  -- | What to file it under. Carries the hash the tarball was verified
  -- against, so a changed tarball misses rather than matching stale data.
  Text ->
  -- | The tarball to read the @.cabal@ file out of.
  FilePath ->
  -- | The package it belongs to, consulted for the hash to verify against.
  PlanPackage ->
  -- | The modules it exposes, or 'Nothing' if the tarball is absent, fails
  -- verification, or holds no @.cabal@ file.
  IO (Maybe [Text])
fromCabalFile cache key tarball p = do
  remembered <- case cache of
    Nothing -> pure Nothing
    Just c -> cachedModules c key
  case remembered of
    -- A cached entry was written after the tarball was verified, and the
    -- key it is filed under contains the hash it was verified against, so a
    -- changed tarball simply misses rather than matching the wrong data.
    Just ms -> pure (Just ms)
    Nothing ->
      verified p tarball >>= \case
        False -> pure Nothing
        True ->
          packageModules tarball >>= \case
            Nothing -> pure Nothing
            Just ms -> do
              case cache of
                Nothing -> pure ()
                Just c -> storeModules c key ms
              pure (Just ms)

-- | How a package's cached answers are filed.
--
-- The expected hash is part of the key, so everything derived from a
-- tarball is bound to the exact bytes it was derived from. A package with
-- no hash in the plan is keyed by name and version alone.
cacheKey :: PlanPackage -> Text
cacheKey p =
  ppName p <> "-" <> ppVersion p <> maybe "" (("-" <>) . T.take 16) (sourceHashOf p)

-- | Does the tarball hash to what the plan says it should?
--
-- Hashing a few megabytes is not free, which is why it happens only on a
-- cache miss: once per package version per machine.
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

-- | Read a module's fixities out of a tarball, following re-exports.
fromSource ::
  -- | How to reach another module, for chasing re-exports. This is
  -- 'resolveModule' tied back on itself, with the visiting set already
  -- extended.
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  -- | Modules currently being resolved, passed through so that a
  -- re-export chain cannot loop.
  Set Text ->
  -- | The tarball holding this module's source.
  FilePath ->
  -- | The module to read.
  Text ->
  -- | Its fixities, including those it only passes on, or 'Nothing' if the
  -- source could not be found or would not parse.
  IO (Maybe (Map OpName Fixity))
fromSource reach visiting tarball modName =
  readModule tarball modName >>= \case
    Nothing -> pure Nothing
    Just source -> fromText reach visiting source modName

-- | The fixities a module's text declares and passes on.
fromText ::
  -- | How to reach another module, for chasing re-exports. This is
  -- 'resolveModule' tied back on itself, with the visiting set already
  -- extended.
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  -- | Modules currently being resolved, passed through so that a
  -- re-export chain cannot loop.
  Set Text ->
  -- | The module's source.
  Text ->
  -- | Its name.
  Text ->
  IO (Maybe (Map OpName Fixity))
fromText reach visiting source modName =
  case parseModule defaultParserConfig (T.unpack modName) (blankCpp source) of
    Left _ -> pure Nothing
    Right pm -> withReexports reach visiting modName (pmModule pm)

-- | Where each module of the project's own packages lives.
--
-- A local package is a directory rather than an archive, so its modules are
-- found by putting the @hs-source-dirs@ of its @.cabal@ file together with
-- the module names it exposes. Nothing is unpacked and nothing is cached:
-- these are the files being worked on.
localModules :: BuildPlan -> IO (Map Text FilePath)
localModules plan =
  Map.unions <$> traverse forPackage [d | LocalPackage d <- map ppSource (bpPackages plan)]
  where
    forPackage dir = quietly Map.empty $ do
      entries <- listDirectory dir
      case filter (".cabal" `isSuffixOf`) entries of
        [] -> pure Map.empty
        (cabalFile : _) -> do
          contents <- readFileText (dir </> cabalFile)
          case contents of
            Nothing -> pure Map.empty
            Just text ->
              Map.fromList . concat
                <$> traverse (locate dir (sourceDirs text)) (exposedModules text)

    -- A package may list several source directories and the @.cabal@ file
    -- does not say which one holds which module, so they are tried in turn
    -- and the first that has the file wins.
    locate dir dirs m = do
      found <- filterM doesFileExist [dir </> T.unpack d </> modulePath m | d <- dirs]
      pure [(m, path) | path <- take 1 found]

    modulePath m = T.unpack (T.replace "." "/" m) <> ".hs"

-- | Read a file, if it is there and is text.
readFileText :: FilePath -> IO (Maybe Text)
readFileText path = quietly Nothing $ do
  there <- doesFileExist path
  if there
    then Just . T.decodeUtf8Lenient <$> BS.readFile path
    else pure Nothing

-- | What a module passes on, as well as what it declares.
--
-- A module that exports an operator it did not declare carries no fixity of
-- its own for it, so the declaration is chased through the export list into
-- whichever module the name came from.
withReexports ::
  -- | How to reach another module, for names this one only passes on.
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  -- | Modules currently being resolved. A candidate already in here is
  -- skipped rather than followed.
  Set Text ->
  -- | The name this module was looked up under, used to recognise a
  -- @module M@ export that refers to the module itself.
  Text ->
  -- | The module, already parsed.
  HsModule GhcPs ->
  -- | What it declares together with what it re-exports, or 'Nothing' if a
  -- module it passes names on from could not be read.
  IO (Maybe (Map OpName Fixity))
withReexports reach visiting modName hsModule =
  case moduleExports hsModule of
    Nothing -> pure (Just own)
    Just items -> do
      visible <-
        if null (wantedNames items)
          then pure (Just [])
          else sequence <$> traverse (fromModule . importModule) (moduleImports hsModule)
      wholeModules <- sequence <$> traverse fromModule (wantedModules items)
      pure $ do
        seen <- visible
        whole <- wholeModules
        let passedOn = Map.restrictKeys (Map.unions seen) (Set.fromList (wantedNames items))
        pure (Map.unions (own : passedOn : whole))
  where
    own = declaredFixities hsModule
    defined = declaredNames hsModule
    wantedNames items = [op | ExportName op <- items, not (Set.member op defined)]
    wantedModules items =
      Set.toList (Set.fromList [m | ExportModule m <- items, not (isSelf m)])
    isSelf m = Just m == moduleName hsModule || m == modName
    fromModule m
      | m `Set.member` visiting = pure (Just Map.empty)
      | otherwise = reach m

-- | Find a module inside a tarball and decode it.
readModule :: FilePath -> Text -> IO (Maybe Text)
readModule tarball modName = quietly Nothing $ do
  bytes <- BL.readFile tarball
  let entries = Tar.read (GZip.decompress bytes)
  pure (Tar.foldEntries step Nothing (const Nothing) entries)
  where
    suffix = "/" <> T.unpack (T.replace "." "/" modName) <> ".hs"
    step entry acc
      | suffix `isSuffixOf` Tar.entryPath entry,
        Tar.NormalFile content _ <- Tar.entryContent entry =
          Just (T.decodeUtf8Lenient (BL.toStrict content))
      | otherwise = acc


-- | How far a chain of re-exports is followed.
reexportDepth :: Int
reexportDepth = 6
----------------------------------------------------------------------------
-- Readiness

-- | Whether everything the resolver needs is on disk.
data Readiness
  = -- | Nothing to do.
    Ready
  | -- | No build plan; @cabal@ has not solved this project yet.
    PlanMissing
  | -- | The plan is older than the files that determine it.
    PlanStale [FilePath]
  | -- | The plan is there but some packages have not been downloaded. The
    -- names are listed so that a caller can say what it is waiting for.
    SourcesMissing [Text]
  deriving (Eq, Show)

-- | Where @cabal@ writes the plan for a project.
planPathFor :: FilePath -> FilePath
planPathFor projectDir = projectDir </> "dist-newstyle" </> "cache" </> "plan.json"

-- | Check what is missing, cheaply.
--
-- One read of the plan and one @stat@ per package, so this is fast enough
-- to run before every format without anyone noticing.
checkReadiness :: FilePath -> IO Readiness
checkReadiness projectDir =
  readBuildPlan (planPathFor projectDir) >>= \case
    Left _ -> pure PlanMissing
    Right plan -> do
      newer <- filesNewerThanPlan projectDir
      if not (null newer)
        then pure (PlanStale newer)
        else do
          tarballs <- plannedTarballs plan
          missing <-
            traverse
              (\(p, t) -> (\there -> if there then Nothing else Just (ppName p)) <$> doesFileExist t)
              tarballs
          pure $ case [n | Just n <- missing] of
            [] -> Ready
            ns -> SourcesMissing ns

-- | The project files that have changed since the plan was written.
--
-- A plan describes the dependencies as they were when @cabal@ last solved.
-- Edit a @build-depends@ and the plan on disk is about a different project,
-- and resolving fixities against it would answer for packages that are no
-- longer in play. Comparing modification times is one @stat@ each, so this
-- costs nothing to check every time.
filesNewerThanPlan :: FilePath -> IO [FilePath]
filesNewerThanPlan projectDir = quietly [] $ do
  planTime <- getModificationTime (planPathFor projectDir)
  entries <- quietly [] (listDirectory projectDir)
  let candidates =
        filter
          (\f -> f `elem` projectFiles || ".cabal" `isSuffixOf` f)
          entries
  newer <- traverse (isNewerThan planTime) candidates
  pure [f | Just f <- newer]
  where
    projectFiles =
      ["cabal.project", "cabal.project.local", "cabal.project.freeze"]
    isNewerThan planTime f = quietly Nothing $ do
      t <- getModificationTime (projectDir </> f)
      pure (if t > planTime then Just f else Nothing)

-- | Do whatever is missing, by asking @cabal@.
--
-- Neither of these builds anything: a dry run only solves, and
-- @--only-download@ only fetches. Both are one-time costs, and @cabal@'s
-- package cache is shared between projects, so a machine that has seen a
-- dependency once never fetches it again.
--
-- This runs a subprocess and may reach the network, so it is a separate
-- call rather than something 'newResolver' does behind the caller's back.
-- An editor formatting on save must not block on it.
prepare :: FilePath -> Readiness -> IO (Either Text ())
prepare projectDir = \case
  Ready -> pure (Right ())
  PlanMissing -> cabal ["build", "--dry-run"]
  PlanStale _ -> cabal ["build", "--dry-run"]
  SourcesMissing _ -> cabal ["build", "--only-download"]
  where
    cabal args = quietly (Left "could not run cabal") $ do
      (code, _, err) <-
        readCreateProcessWithExitCode (proc "cabal" args) {cwd = Just projectDir} ""
      pure $ case code of
        ExitSuccess -> Right ()
        _ -> Left (T.strip (T.pack err))

-- | Get a plan that is safe to use, doing whatever @cabal@ work is needed.
--
-- This is the call most users want. It checks, asks @cabal@ if anything is
-- missing or possibly out of date, and then reads the plan. Once a dry run
-- has succeeded the plan on disk is correct by construction: @cabal@ would
-- have rewritten it otherwise. So the readiness is not consulted a second
-- time, and a project whose files are merely newer than its plan does not
-- send this into a loop.
loadPlan :: FilePath -> IO (Either Text BuildPlan)
loadPlan projectDir = do
  readiness <- checkReadiness projectDir
  prepare projectDir readiness >>= \case
    Left err | readiness /= Ready -> pure (Left err)
    _ -> readBuildPlan (planPathFor projectDir)

-- | Every planned package whose source could be in the package cache, with
-- where that would be.
--
-- Not only the ones the plan will fetch. A package already installed still
-- has a tarball in the cache if anything ever downloaded it, and under Nix
-- that is the normal case for every dependency. A package with no tarball
-- costs one @stat@ and falls through.
--
-- Local packages are excluded: they are directories, not archives.
plannedTarballs :: BuildPlan -> IO [(PlanPackage, FilePath)]
plannedTarballs plan = do
  cacheDir <- packageCacheDir
  pure
    [ (p, tarballFor cacheDir p)
    | p <- bpPackages plan,
      not (isLocal p)
    ]
  where
    isLocal p = case ppSource p of
      LocalPackage _ -> True
      _ -> False

-- | Where @cabal@ keeps downloaded package sources.
packageCacheDir :: IO FilePath
packageCacheDir =
  lookupEnv "CABAL_DIR" >>= \case
    Just dir -> pure (dir </> "packages" </> hackage)
    Nothing -> do
      home <- getHomeDirectory
      let xdg = home </> ".cache" </> "cabal" </> "packages" </> hackage
          legacy = home </> ".cabal" </> "packages" </> hackage
      exists <- doesFileExist (xdg </> "01-index.tar")
      pure (if exists then xdg else legacy)
  where
    hackage = "hackage.haskell.org"

-- | Where a package's source tarball should be.
tarballFor :: FilePath -> PlanPackage -> FilePath
tarballFor cacheDir p =
  cacheDir
    </> T.unpack (ppName p)
    </> T.unpack (ppVersion p)
    </> T.unpack (ppName p <> "-" <> ppVersion p <> ".tar.gz")
