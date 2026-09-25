{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | "Tilia.Fixity" resolves a module's operators exactly, given a function
-- that says what each imported module exports. This is that function, built
-- from what the project itself is compiled against.
module Tilia.Fixity.Plan
  ( -- * Build plans
    PlanPackage (..),
    PackageSource (..),
    isFetchable,
    sourceHashOf,
    BuildPlan (..),
    readBuildPlan,
    tokenForEnvAndBuildPlan,
    tokenForBuildPlan,
    macrosOf,

    -- * Readiness
    Readiness (..),
    PlanComponent (..),
    spellComponent,
    plannedComponents,
    planPathFor,
    checkReadiness,
    plannedTarballs,
    packageCacheRoot,
    guessedPackageCacheRoot,
    Futility (..),
    undiscoveredFutility,
    prepareWith,
    loadPlan,

    -- * Resolving
    Route (..),
    Resolver (..),
    newResolver,
    newResolverVia,
    withReexports,
    scopeFor,
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Applicative ((<|>))
import Control.Monad (filterM, foldM, join)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson
  ( FromJSON (..),
    Value,
    decodeStrict,
    eitherDecodeFileStrict,
    withObject,
    (.:),
    (.:?),
  )
import Data.Aeson.Types (parseMaybe)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BL
import Data.Choice (Choice, fromBool)
import Data.Foldable (toList, traverse_)
import Data.IORef
import Data.List (isSuffixOf)
import Data.List qualified
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Read qualified as T
import GHC.Hs (HsModule)
import GHC.Hs.Extension (GhcPs)
import GHC.IO.Handle (hDuplicate)
import GHC.LanguageExtensions.Type (Extension (ImplicitPrelude))
import System.Directory
  ( XdgDirectory (XdgCache),
    doesFileExist,
    getAppUserDataDirectory,
    getModificationTime,
    getXdgDirectory,
    listDirectory,
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, stderr)
import System.Info qualified
import System.Process
  ( StdStream (Inherit, UseHandle),
    createProcess,
    cwd,
    proc,
    std_err,
    std_out,
    waitForProcess,
  )
import Tilia.Cabal.Package (newPackageReader)
import Tilia.Cpp.Directives (branchLeaves, withoutRuledOut)
import Tilia.Cpp.Macros (Macros (..))
import Tilia.Fixity
import Tilia.Fixity.Builtin (builtinFixities)
import Tilia.Fixity.ByHand (byHandFixities, hscFixities)
import Tilia.Fixity.Cabal
  ( cabalFileAtTop,
    cabalFileInArchive,
    containedModules,
    declaredExtensions,
    entryPosixPath,
    packageModules,
    sourceDirs,
  )
import Tilia.Fixity.Cache
import Tilia.Fixity.Interface
import Tilia.Fixity.PackageDb
import Tilia.Parser
import Tilia.Pragma (effectiveExtensions)
import Tilia.Process (readProgramOutput)
import Tilia.Utils (quietly)

----------------------------------------------------------------------------
-- The plan

-- | One package of a build plan.
data PlanPackage = PlanPackage
  { -- | Package name
    ppName :: Text,
    -- | Package version
    ppVersion :: Text,
    -- | Package source
    ppSource :: PackageSource,
    -- | Which components of the package the entry is about.
    --
    -- Usually one, because @cabal@ configures a package one component at a
    -- time and gives each its own entry. A package it cannot take apart—one
    -- with a @Custom@ build type, whose @Setup.hs@ is entitled to do as it
    -- pleases—is planned whole instead, and its entry is about every
    -- component at once.
    ppComponents :: [Text]
  }
  deriving (Eq, Show)

-- | Where a package's source is.
data PackageSource
  = -- | Already installed, so @cabal@ will not build it.
    --
    -- Not the same as "ships with the compiler", though it includes those.
    PreExisting
  | -- | A directory on this machine—the project being formatted, or a
    -- sibling of it in the same repository.
    LocalPackage FilePath
  | -- | Fetched from a repository as a tarball, with the SHA-256 the plan
    -- expects it to have and where it was fetched from.
    RepoPackage (Maybe Text) RepoProvenance
  | -- | A @source-repository-package@ we have not found the sources of.
    SourceRepo
  | -- | The same, found unpacked under the project's own @dist-newstyle@.
    CheckedOut FilePath
  deriving (Eq, Show)

-- | Which repository a package was fetched from, as far as it bears on
-- finding the tarball afterwards.
data RepoProvenance
  = -- | One @cabal@ downloads from, named by its URI. The tarball goes into
    -- the package cache, in a directory named after the repository as the
    -- configuration spells it.
    RepoDownloaded Text
  | -- | A directory of tarballs, named by @file+noindex@. Nothing is
    -- downloaded and nothing is cached: the tarball is already sitting
    -- there, beside the index @cabal@ wrote for it.
    RepoFromDirectory FilePath
  | -- | A plan that does not say. Older @cabal@ wrote nothing here, and
    -- Hackage is the only guess worth making.
    RepoNoProvenance
  deriving (Eq, Show)

-- | Is there a tarball to go and read?
isFetchable :: PlanPackage -> Bool
isFetchable p = case ppSource p of
  RepoPackage _ _ -> True
  _ -> False

-- | Every package the compiler can see.
getInstalledPackages :: Maybe Cache -> IO [InstalledPackage]
getInstalledPackages cache =
  remembered >>= \case
    Just packages -> pure packages
    Nothing -> do
      found <- readInstalledPackages
      traverse_ (`storeInstalled` found) cache
      pure (installedPackages found)
  where
    remembered = maybe (pure Nothing) cachedInstalled cache

-- | Summarize a 'BuildPlan', and the environment it will be read in, by
-- hashing over both.
tokenForBuildPlan :: BuildPlan -> IO PlanToken
tokenForBuildPlan plan = do
  environment <- compilerIdentity
  pure (tokenForEnvAndBuildPlan environment plan)

-- | Similar to 'tokenForBuildPlan', but allows passing a compiler identity
-- as an argument.
tokenForEnvAndBuildPlan :: Text -> BuildPlan -> PlanToken
tokenForEnvAndBuildPlan environment plan =
  PlanToken
    . T.take 16
    . T.decodeUtf8Lenient
    . B16.encode
    . SHA256.hash
    . T.encodeUtf8
    $ T.intercalate
      "\n"
      (environment : bpCompiler plan : Data.List.sort (fmap cacheKey (bpPackages plan)))

-- | The SHA-256 the plan expects this package's tarball to have.
sourceHashOf :: PlanPackage -> Maybe Text
sourceHashOf p = case ppSource p of
  RepoPackage hash _ -> hash
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
    repo <- o .:? "pkg-src" >>= traverse (.:? "repo")
    repoKind <- traverse (traverse (.:? "type")) repo
    repoUri <- traverse (traverse (.:? "uri")) repo
    repoPath <- traverse (traverse (.:? "path")) repo
    named <- o .:? "component-name"
    whole <- o .:? "components"
    pure
      PlanPackage
        { ppName = name,
          ppVersion = version,
          ppComponents = componentsOf named whole,
          ppSource = case (kind :: Maybe Text, sourceKind :: Maybe Text) of
            (Just "pre-existing", _) -> PreExisting
            (_, Just "repo-tar") ->
              RepoPackage
                sourceHash
                ( case (join (join repoKind) :: Maybe Text, join (join repoPath), join (join repoUri)) of
                    (Just "local-repo-no-index", Just dir, _) -> RepoFromDirectory (T.unpack dir)
                    (_, _, Just uri) -> RepoDownloaded uri
                    _ -> RepoNoProvenance
                )
            (_, Just "local") -> LocalPackage (maybe "" T.unpack (join sourcePath))
            (_, Just "source-repo") -> SourceRepo
            -- Anything else is treated as already present.
            _ -> PreExisting
        }

-- | The components one plan entry is about.
--
-- @cabal@ writes this two ways. An entry for a single component names it in
-- @component-name@; an entry for a package planned whole carries a
-- @components@ object instead, keyed by the very same spellings. Reading
-- only the first would leave every @Custom@ package looking like one the
-- plan says nothing about, and a run would keep asking @cabal@ to solve
-- again for components a fresh solve would file exactly where this one did.
--
-- @setup@ is dropped. It is the @Setup.hs@ program @cabal@ builds in order
-- to build the package, not a component of the package, and nothing in the
-- project will ever be formatted as part of it.
componentsOf :: Maybe Text -> Maybe (Map Text Value) -> [Text]
componentsOf named whole = case named of
  Just component -> [component]
  Nothing -> filter (/= "setup") (Map.keys (fromMaybe Map.empty whole))

-- | Read @plan.json@.
readBuildPlan :: FilePath -> IO (Either Text BuildPlan)
readBuildPlan path =
  doesFileExist path >>= \case
    False -> pure (Left ("no build plan at " <> T.pack path))
    True ->
      eitherDecodeFileStrict path >>= \case
        Left why -> pure (Left (T.pack why))
        Right plan -> Right <$> checkedOutIn (takeDirectory (takeDirectory path)) plan

-- | Find where @cabal@ unpacked each @source-repository-package@.
checkedOutIn :: FilePath -> BuildPlan -> IO BuildPlan
checkedOutIn distDir plan = do
  packages <- traverse locate (bpPackages plan)
  pure plan{bpPackages = packages}
  where
    locate p = case ppSource p of
      SourceRepo ->
        clonesOf p >>= \case
          (dir : _) -> pure p{ppSource = CheckedOut dir}
          [] -> pure p
      _ -> pure p
    clonesOf p = quietly [] $ do
      entries <- listDirectory (distDir </> "src")
      filterM
        (isThePackage p)
        [ distDir </> "src" </> e
        | e <- Data.List.sort entries,
          (ppName p <> "-") `T.isPrefixOf` T.pack e
        ]
    isThePackage p dir = quietly False $ do
      contents <- readFileText (dir </> T.unpack (ppName p) <> ".cabal")
      pure (maybe False (describes p) contents)
    describes p text =
      any (names "name:" (ppName p)) (T.lines text)
        && any (names "version:" (ppVersion p)) (T.lines text)
    names field value line = case T.stripPrefix field (T.toLower (T.strip line)) of
      Just rest -> T.strip rest == T.toLower value
      Nothing -> False

-- | The version macros a plan settles.
macrosOf :: BuildPlan -> Macros
macrosOf plan =
  Macros
    { macroVersions =
        Map.fromList
          ( [ ("MIN_VERSION_" <> underscored name, version)
            | (name, [version]) <- Map.toList (Map.map Set.toList versions)
            ]
              <> [("MIN_VERSION_GLASGOW_HASKELL", v) | v <- toList compiler]
          ),
      macroNumbers =
        Map.fromList
          [ entry
          | (major : minor : patches) <- toList compiler,
            entry <-
              [ ("__GLASGOW_HASKELL__", major * 100 + minor),
                ("__GLASGOW_HASKELL_PATCHLEVEL1__", nth 0 patches),
                ("__GLASGOW_HASKELL_PATCHLEVEL2__", nth 1 patches)
              ]
          ]
    }
  where
    versions =
      Map.fromListWith
        Set.union
        [ (ppName p, Set.singleton v)
        | p <- bpPackages plan,
          Just v <- [numberedVersion (ppVersion p)]
        ]
    compiler = do
      version <- T.stripPrefix "ghc-" (bpCompiler plan)
      parts <- numberedVersion version
      case parts of
        _ : _ : _ -> Just (take 4 (parts <> repeat 0))
        _ -> Nothing
    nth i xs = if i < length xs then xs !! i else 0

-- | The modules @cabal@ writes itself for a plan's packages.
generatedModules :: BuildPlan -> Set Text
generatedModules plan =
  Set.fromList
    [ prefix <> underscored (ppName p)
    | p <- bpPackages plan,
      prefix <- ["Paths_", "PackageInfo_"]
    ]

-- | A package's name as a module name spells it, which is with the hyphens
-- turned into underscores. @cabal@ does this for the version macros and for
-- the modules it generates alike.
underscored :: Text -> Text
underscored = T.map (\c -> if c == '-' then '_' else c)

-- | A version as its numbers, or 'Nothing' where any of them is not one.
numberedVersion :: Text -> Maybe [Integer]
numberedVersion = traverse number . T.splitOn "."
  where
    number part = case T.decimal part of
      Right (n, rest) | T.null rest -> Just n
      _ -> Nothing

----------------------------------------------------------------------------
-- Readiness

-- | A component of the project.
data PlanComponent = PlanComponent
  { -- | The package it belongs to.
    pcPackage :: Text,
    -- | @lib@, @exe:name@, @test:name@, @bench:name@.
    pcName :: Text
  }
  deriving (Eq, Ord, Show)

-- | A component as it would be written on the command line.
spellComponent :: PlanComponent -> Text
spellComponent c = pcPackage c <> ":" <> pcName c

-- | The components of the project's own packages that a plan covers.
plannedComponents :: BuildPlan -> [PlanComponent]
plannedComponents plan =
  [ PlanComponent (ppName p) component
  | p <- bpPackages plan,
    LocalPackage _ <- [ppSource p],
    component <- ppComponents p
  ]

-- | Whether everything the resolver needs is on disk.
data Readiness
  = -- | Nothing to do.
    Ready
  | -- | No build plan; @cabal@ has not solved this project yet.
    PlanMissing
  | -- | The plan is older than the files that determine it.
    PlanStale [FilePath]
  | -- | The plan says nothing about components the run is about to format.
    PlanNarrow [Text]
  | -- | The plan is there, and some packages have neither been downloaded
    -- nor built. The names are listed so that a caller can say what it is
    -- waiting for.
    --
    -- Built counts as having them: their interfaces provide everything the
    -- source tarball would.
    SourcesMissing [Text]
  deriving (Eq, Show)

-- | Where @cabal@ writes the plan for a project.
planPathFor :: FilePath -> FilePath
planPathFor projectDir = projectDir </> "dist-newstyle" </> "cache" </> "plan.json"

-- | Check what is missing.
--
-- One read of the plan and one @stat@ per package, so this is fast enough
-- to run before every format.
checkReadiness :: [PlanComponent] -> FilePath -> IO Readiness
checkReadiness wanted projectDir =
  readBuildPlan (planPathFor projectDir) >>= \case
    Left _ -> pure PlanMissing
    Right plan -> do
      newer <- filesNewerThanPlan plan projectDir
      let covered = plannedComponents plan
          missing = [spellComponent c | c <- wanted, c `notElem` covered]
      case (newer, missing) of
        (_ : _, _) -> pure (PlanStale newer)
        ([], _ : _) -> pure (PlanNarrow missing)
        ([], []) ->
          sourcesShortOf plan >>= \case
            [] -> pure Ready
            ns -> pure (SourcesMissing ns)

-- | The packages the plan expects to fetch whose sources are not here.
sourcesShortOf :: BuildPlan -> IO [Text]
sourcesShortOf plan = do
  tarballs <- filter (isFetchable . fst) <$> plannedTarballs plan
  absent <- fmap fst <$> filterM (fmap not . doesFileExist . snd) tarballs
  short <-
    if null absent
      then pure []
      else do
        cache <- openCache =<< tokenForBuildPlan plan
        installed <- getInstalledPackages cache
        pure (filter (not . builtAlready installed) absent)
  pure (fmap ppName short)

-- | Has the compiler got this package already?
builtAlready :: [InstalledPackage] -> PlanPackage -> Bool
builtAlready installed p = any matches installed
  where
    matches i = ipName i == ppName p && ipVersion i == ppVersion p

-- | The project files that have changed since the plan was written.
filesNewerThanPlan :: BuildPlan -> FilePath -> IO [FilePath]
filesNewerThanPlan plan projectDir = quietly [] $ do
  planTime <- getModificationTime (planPathFor projectDir)
  atRoot <- quietly [] (listDirectory projectDir)
  inPackages <- concat <$> traverse cabalFilesIn (localDirs plan)
  let candidates =
        [projectDir </> f | f <- atRoot, f `elem` projectFiles]
          <> [projectDir </> f | f <- atRoot, ".cabal" `isSuffixOf` f]
          <> inPackages
  newer <- traverse (isNewerThan planTime) candidates
  pure [f | Just f <- newer]
  where
    projectFiles =
      ["cabal.project", "cabal.project.local", "cabal.project.freeze"]
    localDirs p =
      Data.List.nub [dir | LocalPackage dir <- fmap ppSource (bpPackages p)]
    cabalFilesIn dir = quietly [] $ do
      entries <- listDirectory dir
      pure [dir </> f | f <- entries, ".cabal" `isSuffixOf` f]
    isNewerThan planTime path = quietly Nothing $ do
      t <- getModificationTime path
      pure (if t > planTime then Just path else Nothing)

-- | Do whatever is missing, by asking @cabal@.
prepare :: [PlanComponent] -> FilePath -> Readiness -> IO (Either Text ())
prepare wanted projectDir readiness =
  prepareWith
    (runCabal projectDir)
    (futilityFor projectDir)
    wanted
    projectDir
    readiness

-- | An account of actions we know are not worth attempting.
data Futility = Futility
  { -- | Has solving this plan already been tried and left it as narrow?
    solveWasFutile :: IO Bool,
    -- | Record that a solve has been run and left it narrow, which is what
    -- 'solveWasFutile' answers from afterwards.
    rememberFutileSolve :: IO (),
    -- | The packages an earlier fetch was still short of afterwards.
    fetchWasFutileFor :: IO [Text],
    -- | Remember what a fetch left missing.
    rememberFutileFetch :: [Text] -> IO ()
  }

-- | The state when we know nothing about futile actions yet.
undiscoveredFutility :: Futility
undiscoveredFutility =
  Futility
    { solveWasFutile = pure False,
      rememberFutileSolve = pure (),
      fetchWasFutileFor = pure [],
      rememberFutileFetch = const (pure ())
    }

-- | A memory kept in the cache, under the plan the project has now.
futilityFor :: FilePath -> Futility
futilityFor projectDir =
  Futility
    { solveWasFutile = withCache False cachedFutileSolve,
      rememberFutileSolve = withCache () storeFutileSolve,
      fetchWasFutileFor = withCache [] cachedFutileFetch,
      rememberFutileFetch = \packages -> withCache () (`storeFutileFetch` packages)
    }
  where
    withCache fallback use =
      readBuildPlan (planPathFor projectDir) >>= \case
        Left _ -> pure fallback
        Right plan -> do
          opened <- openCache =<< tokenForBuildPlan plan
          maybe (pure fallback) use opened

-- | 'prepare', given a way to run @cabal@ and a memory of what earlier
-- attempts came to.
prepareWith ::
  -- | Run @cabal@ with these arguments.
  ([String] -> IO (Either Text ())) ->
  -- | What earlier attempts came to.
  Futility ->
  -- | The components the run is about to format.
  [PlanComponent] ->
  -- | The project being prepared.
  FilePath ->
  -- | What it was found to be short of.
  Readiness ->
  IO (Either Text ())
prepareWith cabal futility wanted projectDir = \case
  Ready -> pure (Right ())
  SourcesMissing _ -> fetch
  PlanMissing -> solveThenFetch
  PlanStale _ -> solveThenFetch
  PlanNarrow _ ->
    solveWasFutile futility >>= \case
      True -> fetchWhatIsShort
      False -> solveThenFetch
  where
    wholeProject = ["--enable-tests", "--enable-benchmarks"]
    tryWholeProject args =
      cabal (args <> wholeProject) >>= \case
        Right () -> pure (Right ())
        Left _ -> cabal args
    fetch = tryWholeProject ["build", "all", "--only-download"]
    fetchWhatIsShort =
      readBuildPlan (planPathFor projectDir) >>= \case
        Left _ -> pure (Right ())
        Right plan -> do
          short <- sourcesShortOf plan
          refused <- fetchWasFutileFor futility
          if null short || all (`elem` refused) short
            then pure (Right ())
            else
              fetch >>= \case
                Left err -> pure (Left err)
                Right () -> do
                  left <- sourcesShortOf plan
                  rememberFutileFetch futility left
                  pure (Right ())
    solveThenFetch =
      tryWholeProject ["build", "all", "--dry-run"] >>= \case
        Left err -> pure (Left err)
        Right () ->
          checkReadiness wanted projectDir >>= \case
            SourcesMissing _ -> fetch
            PlanNarrow _ -> rememberFutileSolve futility >> fetchWhatIsShort
            _ -> pure (Right ())

-- | Run @cabal@ in a project directory, letting it speak for itself.
runCabal :: FilePath -> [String] -> IO (Either Text ())
runCabal projectDir args = quietly (Left "could not run cabal") $ do
  hFlush stderr
  -- A duplicate because 'createProcess' closes the handle it is given once
  -- the child has it, and closing the real standard error would leave
  -- nothing to report the failure on.
  passed <- hDuplicate stderr
  (_, _, _, running) <-
    createProcess
      (proc "cabal" args)
        { cwd = Just projectDir,
          std_out = UseHandle passed,
          std_err = Inherit
        }
  code <- waitForProcess running
  pure $ case code of
    ExitSuccess -> Right ()
    _ -> Left ("cabal " <> T.unwords (fmap T.pack args) <> " failed; see above")

-- | Get a plan that is safe to use, doing whatever @cabal@ work is needed.
loadPlan :: [PlanComponent] -> FilePath -> IO (Either Text BuildPlan)
loadPlan wanted projectDir = do
  readiness <- checkReadiness wanted projectDir
  prepare wanted projectDir readiness >>= \case
    Left err -> pure (Left err)
    _ -> readBuildPlan (planPathFor projectDir)

-- | Every planned package whose source could be in the package cache, with
-- where that would be.
plannedTarballs :: BuildPlan -> IO [(PlanPackage, FilePath)]
plannedTarballs plan = do
  cacheRoot <- packageCacheRoot
  repos <- quietly [] (Data.List.sort <$> listDirectory cacheRoot)
  traverse
    (\p -> (,) p <$> tarballFor cacheRoot repos p)
    [p | p <- bpPackages plan, not (isLocal p)]
  where
    isLocal p = case ppSource p of
      LocalPackage _ -> True
      CheckedOut _ -> True
      _ -> False

-- | Where @cabal@ keeps downloaded package sources, one directory per
-- repository it downloads from.
packageCacheRoot :: IO FilePath
packageCacheRoot =
  readProgramOutput
    "cabal"
    ["path", "--remote-repo-cache", "--output-format=json"]
    >>= \case
      Just said | Just dir <- remoteRepoCacheIn said -> pure dir
      _ -> guessedPackageCacheRoot

-- | The package cache directory, out of what @cabal path@ printed.
remoteRepoCacheIn :: Text -> Maybe FilePath
remoteRepoCacheIn said = do
  spoken <-
    listToMaybe $
      reverse (filter (not . T.null) (fmap T.strip (T.lines said)))
  value <- decodeStrict (T.encodeUtf8 spoken)
  parseMaybe (withObject "cabal path" (.: "remote-repo-cache")) value

-- | Where @cabal@ probably keeps its downloaded packages.
guessedPackageCacheRoot :: IO FilePath
guessedPackageCacheRoot =
  lookupEnv "CABAL_DIR" >>= \case
    Just dir -> pure (dir </> "packages")
    Nothing -> do
      places <- cabalDirs
      found <- filterM holdsAnIndex (toList places)
      pure (fromMaybe (NE.head places) (listToMaybe found))
  where
    holdsAnIndex dir =
      quietly False (doesFileExist (dir </> hackage </> "01-index.tar"))
    hackage = "hackage.haskell.org"

-- | Every directory @cabal@ could be keeping a package cache in, the
-- platform's own default first.
cabalDirs :: IO (NonEmpty FilePath)
cabalDirs = do
  appData <- getAppUserDataDirectory "cabal"
  xdg <- quietly Nothing (Just <$> getXdgDirectory XdgCache "cabal")
  pure . fmap (</> "packages") $ case xdg of
    Just dir | not onWindows -> dir :| [appData]
    Just dir -> appData :| [dir]
    Nothing -> appData :| []

-- | Whether this is a Windows build, for the places that differ there.
onWindows :: Bool
onWindows = System.Info.os == "mingw32"

-- | The repository @cabal@ would have kept a package's sources under.
hackageByDefault :: FilePath
hackageByDefault = "hackage.haskell.org"

-- | Where a package's source tarball is, or where fetching would put it.
tarballFor :: FilePath -> [FilePath] -> PlanPackage -> IO FilePath
tarballFor cacheRoot repos p = case provenanceOf p of
  RepoFromDirectory dir -> pure (dir </> flat)
  RepoDownloaded uri -> searched (hostOf uri)
  RepoNoProvenance -> searched Nothing
  where
    flat = T.unpack (ppName p <> "-" <> ppVersion p <> ".tar.gz")
    under repo =
      cacheRoot
        </> repo
        </> T.unpack (ppName p)
        </> T.unpack (ppVersion p)
        </> flat
    searched preferred = do
      let first' = fromMaybe hackageByDefault preferred
          rest = filter (/= first') repos
      found <- filterM doesFileExist (fmap under (first' : rest))
      pure (fromMaybe (under first') (listToMaybe found))

-- | Which repository a package came from, where it came from one.
provenanceOf :: PlanPackage -> RepoProvenance
provenanceOf p = case ppSource p of
  RepoPackage _ repo -> repo
  _ -> RepoNoProvenance

-- | The host a URI names, which is what @cabal@ conventionally calls the
-- repository that lives there.
hostOf :: Text -> Maybe FilePath
hostOf uri = case T.breakOn "//" uri of
  (_, rest)
    | not (T.null rest),
      host <- T.takeWhile (/= '/') (T.drop 2 rest),
      not (T.null host) ->
        Just (T.unpack host)
  _ -> Nothing

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
data Resolver = Resolver
  { -- | What a module exports, with 'Nothing' for one that could not be
    -- read, which is not the same as its having no operators; see
    -- 'Tilia.Fixity.resolveScope' for why the difference has to survive.
    askFixities :: Text -> IO (Maybe (Fixities)),
    -- | What a module keeps under each of its names, for the sake of a
    -- @T(..)@ in an import list.
    askChildren :: Text -> IO (Map OpName (Set OpName)),
    -- | The operators an unread module's export list names, asked only of
    -- the modules 'askFixities' gave up on, and what keeps a module that
    -- plainly has no such operator from being blamed for one.
    askExportNames :: Text -> IO (Maybe (Set OpName)),
    -- | The modules reading a module went through before giving up, the one
    -- it gave up on last. Asked only of the modules 'askFixities' gave up
    -- on, and only so that a message can name the exact problematic module
    -- rather than the import that happens to sit above it.
    askChain :: Text -> IO [Text]
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
newResolver = newResolverVia [FromInterface, FromSource]

-- | 'newResolver', restricted to the routes given.
newResolverVia ::
  -- | Which readings to try, in order.
  [Route] ->
  -- | The build plan to use.
  BuildPlan ->
  IO Resolver
newResolverVia routes plan = do
  tarballs <- plannedTarballs plan
  cache <- openCache =<< tokenForBuildPlan plan
  installed <- getInstalledPackages cache
  index <- buildModuleIndex cache installed tarballs
  let interfaces = interfaceIndex installed
  local <- localModules plan
  memo <- newIORef Map.empty
  childrenRead <- newIORef Map.empty
  askPackage <- newPackageReader
  extensionsRead <- newIORef Map.empty
  exportsRead <- newIORef Map.empty
  interfacesRead <- newIORef Map.empty
  let interfaceOf modName = do
        seen <- readIORef interfacesRead
        case Map.lookup modName seen of
          Just interface -> pure interface
          Nothing -> do
            found <- case Map.lookup modName interfaces of
              Nothing -> pure Nothing
              Just (_, path) -> readInterface modName path
            -- Being listed is not the same as being readable: @ghc-pkg@
            -- names @GHC.Prim@ among @ghc-prim@'s modules and there is no
            -- file at the path that implies. So the table answers for a
            -- module with nothing to read, however it came to have nothing.
            let interface = case found of
                  Just _ -> found
                  Nothing -> asInterface <$> Map.lookup modName builtinFixities
            atomicModifyIORef' interfacesRead (\m -> (Map.insert modName interface m, ()))
            pure interface
  let workings =
        Workings
          { wkRoutes = routes,
            wkCache = cache,
            wkLocal = local,
            wkIndex = index,
            wkInterfaces = interfaces,
            wkInterfaceOf = interfaceOf,
            wkReach = reach,
            wkReachChildren = children,
            wkReachExports = exports,
            wkExtensionsOf = extensionsOf,
            wkMacros = macrosOf plan,
            wkGenerated = generatedModules plan
          }
      resolved visiting modName = do
        known <- readIORef memo
        case Map.lookup modName known of
          Just answer -> pure answer
          Nothing -> do
            answer <- resolveModule workings visiting modName
            atomicModifyIORef' memo (\m -> (Map.insert modName answer m, ()))
            pure answer
      reach visiting modName
        | modName `Set.member` visiting = pure Nothing
        | otherwise = fixitiesEstablished <$> resolved visiting modName
      chain visiting = go Set.empty
        where
          go seen modName
            | modName `Set.member` seen = pure []
            | otherwise =
                resolved visiting modName >>= \case
                  Unreadable (Just below) ->
                    (below :) <$> go (Set.insert modName seen) below
                  _ -> pure []
      exports visiting modName
        | modName `Set.member` visiting = pure Nothing
        | otherwise = do
            seen <- readIORef exportsRead
            case Map.lookup modName seen of
              Just names -> pure names
              Nothing -> do
                names <- exportNamesOfModule workings visiting modName
                atomicModifyIORef' exportsRead (\m -> (Map.insert modName names m, ()))
                pure names
      extensionsOf modName
        | Just path <- Map.lookup modName local =
            either (const []) id <$> askPackage path
        | Just (package, tarball) <- Map.lookup modName index = do
            seen <- readIORef extensionsRead
            case Map.lookup package seen of
              Just extensions -> pure extensions
              Nothing -> do
                extensions <- fromTarball tarball
                atomicModifyIORef' extensionsRead (\m -> (Map.insert package extensions m, ()))
                pure extensions
        | otherwise = pure []
      fromTarball tarball =
        quietly [] $ do
          bytes <- BL.readFile tarball
          pure (foldMap declaredExtensions (cabalFileInArchive (Tar.read (GZip.decompress bytes))))
      children visiting modName
        | modName `Set.member` visiting = pure Map.empty
        | otherwise = do
            seen <- readIORef childrenRead
            case Map.lookup modName seen of
              Just kept -> pure kept
              Nothing -> do
                kept <- childrenOfModule workings visiting modName
                atomicModifyIORef' childrenRead (\m -> (Map.insert modName kept m, ()))
                pure kept
  pure
    Resolver
      { askFixities = reach Set.empty,
        askChildren = children Set.empty,
        askExportNames = exports Set.empty,
        askChain = chain Set.empty
      }

-- | The operators a module's export list names, following what it
-- reexports.
--
-- Asked only about modules whose fixities could not be established, and
-- only to decide which of them an unsettled operator can be blamed on. A
-- module that hands on one nobody could read answers 'Nothing'; one with no
-- export list exports what it declares, which is every fixity it could
-- supply.
exportNamesOfModule :: Workings -> Set Text -> Text -> IO (Maybe (Set OpName))
exportNamesOfModule
  Workings{wkCache, wkLocal, wkIndex, wkReachChildren, wkReachExports, wkMacros}
  visiting
  modName
    | Just path <- Map.lookup modName wkLocal,
      writtenForHsc path =
        pure (Just (hscSupplies modName))
    | Just path <- Map.lookup modName wkLocal = namesIn =<< readFileText path
    | Just (package, tarball) <- Map.lookup modName wkIndex =
        remembered package >>= \case
          Just answer -> pure (exportedNames answer)
          Nothing ->
            readModule tarball modName >>= \case
              Nothing -> pure Nothing
              Just ForHsc -> do
                let names = Just (hscSupplies modName)
                store package (asExported names)
                pure names
              Just (Haskell text) -> do
                names <- namesIn (Just text)
                store package (asExported names)
                pure names
    | otherwise = pure Nothing
    where
      remembered package = case wkCache of
        Nothing -> pure Nothing
        Just c -> cachedExportNames c package modName
      store package answer = case wkCache of
        Nothing -> pure ()
        Just c -> storeExportNames c package modName answer
      namesIn text = case parsedLeaves =<< text of
        Nothing -> pure Nothing
        Just modules ->
          fmap Set.unions . sequence <$> traverse readOne modules
      readOne (implicitPrelude, hsModule) =
        exportNamesWithReexports
          implicitPrelude
          (wkReachExports visiting')
          (wkReachChildren visiting')
          modName
          hsModule
      visiting' = Set.insert modName visiting
      parsedLeaves = configurationsOf wkMacros Nothing modName

-- | What a module keeps under each of its names, so that a @T(..)@ in an
-- import list can be told what it brings in.
childrenOfModule :: Workings -> Set Text -> Text -> IO (Map OpName (Set OpName))
childrenOfModule
  Workings
    { wkRoutes,
      wkCache,
      wkLocal,
      wkIndex,
      wkInterfaces,
      wkInterfaceOf,
      wkReachChildren,
      wkExtensionsOf,
      wkMacros
    }
  visiting
  modName
    | Just path <- Map.lookup modName wkLocal,
      writtenForHsc path =
        pure Map.empty
    | Just path <- Map.lookup modName wkLocal =
        readFileText path >>= \case
          Nothing -> pure Map.empty
          Just text -> inSource text
    | otherwise = firstAnswer (fmap taking wkRoutes)
    where
      taking = \case
        FromInterface -> case Map.lookup modName wkInterfaces of
          Nothing -> pure Nothing
          Just (key, _) -> keptUnder key outOfInterface
        FromSource -> case Map.lookup modName wkIndex of
          Nothing -> pure Nothing
          Just (package, tarball) ->
            keptUnder package $
              readModule tarball modName >>= \case
                Nothing -> pure Nothing
                Just ForHsc -> pure (Just Map.empty)
                Just (Haskell text) -> Just <$> inSource text
      firstAnswer [] = pure Map.empty
      firstAnswer (route : rest) =
        route >>= \case
          Just kept -> pure kept
          Nothing -> firstAnswer rest
      keptUnder package readIt =
        remembered package >>= \case
          Just kept -> pure (Just kept)
          Nothing -> do
            kept <- readIt
            traverse_ (store package) kept
            pure kept
      remembered package = case wkCache of
        Nothing -> pure Nothing
        Just c -> cachedChildren c package modName
      store package kept = case wkCache of
        Nothing -> pure ()
        Just c -> storeChildren c package modName kept
      outOfInterface = fmap interfaceChildren <$> wkInterfaceOf modName
      inSource text = do
        extensions <- wkExtensionsOf modName
        case parsedLeaves extensions text of
          Nothing -> pure Map.empty
          Just modules ->
            Map.unionsWith Set.union
              <$> traverse readOne modules
      readOne (implicitPrelude, hsModule) =
        childrenWithReexports
          implicitPrelude
          (wkReachChildren visiting')
          modName
          hsModule
      visiting' = Set.insert modName visiting
      parsedLeaves extensions = configurationsOf wkMacros (Just extensions) modName

-- | Work out what a module can see, using a resolver to reach its imports.
--
-- This is the join between the pure half of "Tilia.Fixity" and the half
-- that touches the disk: the imports are resolved first, and the scope is
-- then computed from the answers. Note that an import the resolver could
-- not read arrives as 'Nothing' and stays 'Nothing', which is what lets
-- 'Tilia.Fixity.lookupFixity' distinguish a conclusion from a guess.
scopeFor ::
  -- | What can be asked about the modules it imports.
  Resolver ->
  -- | Whether @ImplicitPrelude@ is on, which the module's own pragmas
  -- and its package's @default-extensions@ decide between them.
  Choice "implicitPrelude" ->
  -- | The module whose scope is wanted, already parsed.
  HsModule GhcPs ->
  -- | Everything that module can see, and what it could not find out.
  IO Scope
scopeFor resolver implicitPrelude hsModule = do
  let imports = moduleImports implicitPrelude hsModule
  answers <- traverse (\m -> (m,) <$> askFixities resolver m) (fmap importModule imports)
  let table = Map.fromList answers
      unread = [m | (m, Nothing) <- answers]
  names <- Map.fromList <$> traverse (\m -> (m,) <$> askExportNames resolver m) unread
  chains <- Map.fromList <$> traverse (\m -> (m,) <$> askChain resolver m) unread
  kept <-
    Map.fromList
      <$> traverse
        (\m -> (m,) <$> askChildren resolver m)
        (Set.toList (Set.fromList (fmap importModule (filter expands imports))))
  pure $
    resolveScope
      implicitPrelude
      KnownModules
        { knownFixities = \m -> Map.findWithDefault Nothing m table,
          knownChildren = \m -> Map.findWithDefault Map.empty m kept,
          knownExportNames = \m -> Map.findWithDefault Nothing m names,
          knownChain = \m -> Map.findWithDefault [] m chains
        }
      hsModule
  where
    expands i = case importNames i of
      Nothing -> False
      Just (_, items) -> any isAll items
    isAll = \case
      ImportedAll _ -> True
      _ -> False

-- | Everything a resolver consults, and the way back into it.
--
-- None of it changes from one module to the next, which is why
-- 'newResolverVia' builds it once and hands it over whole. What comes back
-- out of that is the 'Resolver'; this is what is behind it.
data Workings = Workings
  { -- | Which readings to try, in the order given.
    wkRoutes :: [Route],
    -- | Where to remember answers between runs.
    wkCache :: Maybe Cache,
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
    -- 'newResolverVia', so that the memo it keeps covers the recursive
    -- calls too.
    wkReach :: Set Text -> Text -> IO (Maybe (Fixities)),
    -- | How to reach another module for what its names carry with them,
    -- tied back the same way and against a visiting set of its own.
    wkReachChildren :: Set Text -> Text -> IO (Map OpName (Set OpName)),
    -- | How to reach another module for what its export list names, tied
    -- back the same way again.
    wkReachExports :: Set Text -> Text -> IO (Maybe (Set OpName)),
    -- | What extensions the package a module belongs to puts in force.
    wkExtensionsOf :: Text -> IO [Extension],
    -- | Known macro expansions.
    wkMacros :: Macros,
    -- | The modules @cabal@ writes itself, which are therefore in no
    -- package's sources. See 'generatedModules'.
    wkGenerated :: Set Text
  }

-- | Where a module's fixities come from, in order of cost.
resolveModule ::
  -- | Where to look, and how to get back to the resolver.
  Workings ->
  -- | Modules currently being resolved further up the call chain.
  Set Text ->
  -- | The module to resolve.
  Text ->
  -- | Its operator fixities, or, where they could not be established, the
  -- module below it that stopped us.
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
      wkReachChildren,
      wkExtensionsOf,
      wkMacros,
      wkGenerated
    }
  visiting
  modName
    | Just builtin <- Map.lookup modName builtinFixities = pure (Declares builtin)
    | Just path <- Map.lookup modName wkLocal,
      writtenForHsc path =
        pure (hscDeclares modName)
    | Just path <- Map.lookup modName wkLocal =
        readFileText path >>= \case
          Nothing -> pure (Unreadable Nothing)
          Just source -> do
            extensions <- wkExtensionsOf modName
            fromText
              wkMacros
              extensions
              (wkReach visiting')
              (wkReachChildren visiting')
              visiting'
              source
              modName
    | otherwise = answered <$> firstAnswer (fmap taking wkRoutes)
    where
      visiting' = Set.insert modName visiting

      taking = \case
        FromInterface -> viaInterface
        FromSource -> viaArchive

      firstAnswer = go Nothing
        where
          go blamed [] = pure (Unreadable blamed)
          go blamed (route : rest) =
            route >>= \case
              Just (Declares fixities) -> pure (Declares fixities)
              Just (Unreadable below) -> go (blamed <|> below) rest
              Nothing -> go blamed rest

      viaInterface = case Map.lookup modName wkInterfaces of
        Nothing -> pure Nothing
        Just (key, _) ->
          cachedFor key >>= \case
            Just remembered -> pure (Just remembered)
            Nothing -> do
              established <- fromInterface wkInterfaceOf modName
              storeFor key established
              pure (Just established)

      viaArchive = case Map.lookup modName wkIndex of
        Nothing -> pure Nothing
        Just (package, tarball) ->
          cachedFor package >>= \case
            Just remembered -> pure (Just remembered)
            Nothing -> do
              extensions <- wkExtensionsOf modName
              fromSource
                wkMacros
                extensions
                (wkReach visiting')
                (wkReachChildren visiting')
                visiting'
                tarball
                modName
                >>= \case
                  NoArchive -> pure Nothing
                  FromArchive established -> do
                    storeFor package established
                    pure (Just established)

      answered = \case
        Declares fixities -> Declares fixities
        Unreadable below
          | Set.member modName wkGenerated -> Declares Map.empty
          | otherwise -> maybe (Unreadable below) Declares (byHand modName)
      byHand = fmap inBothNamespaces . (`Map.lookup` byHandFixities)
      cachedFor package = case wkCache of
        Nothing -> pure Nothing
        Just c -> cachedFixities c package modName
      storeFor package fixities = case wkCache of
        Nothing -> pure ()
        Just c -> storeFixities c package modName fixities

-- | Which package and tarball holds each module.
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
    [ (m, (key, dir </> T.unpack (T.replace "." "/" m) <> ".hi"))
    | i <- installed,
      dir <- ipImportDirs i,
      -- Bound out here so that the directory is hashed once rather than
      -- once for each of the modules found in it.
      let key = keyFor dir,
      m <- ipModules i
    ]
  where
    keyFor dir =
      "interface-"
        <> T.take 24 (T.decodeUtf8Lenient (B16.encode (SHA256.hash (T.encodeUtf8 (T.pack dir)))))

-- | Present a fixity map as an 'Interface'.
asInterface :: Fixities -> Interface
asInterface fixities =
  Interface
    { interfaceDeclares = fixities,
      interfaceReexports = [],
      interfaceChildren = Map.empty
    }

-- | The fixities a compiled interface reports, and those it passes on.
fromInterface ::
  -- | A module's interface, if it has one.
  (Text -> IO (Maybe Interface)) ->
  -- | The module to read.
  Text ->
  IO Established
fromInterface interfaceOf modName =
  interfaceOf modName >>= \case
    Nothing -> pure (Unreadable Nothing)
    Just iface -> do
      declarers <- traverse asked (distinct (fmap fst (interfaceReexports iface)))
      pure $ case [m | (m, Nothing) <- declarers] of
        (m : _) -> Unreadable (Just m)
        [] ->
          Declares . Map.union (interfaceDeclares iface) . Map.unions $
            [ Map.filterWithKey (\(_, o) _ -> o == op) (interfaceDeclares declarer)
            | (m, op) <- interfaceReexports iface,
              Just (Just declarer) <- [lookup m declarers]
            ]
  where
    asked m = do
      interface <- interfaceOf m
      pure (m, interface)
    distinct = Map.keys . Map.fromList . fmap (,())

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
cacheKey :: PlanPackage -> Text
cacheKey p =
  ppName p <> "-" <> ppVersion p <> maybe "" (("-" <>) . T.take 16) (sourceHashOf p)

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

-- | Read a module's fixities out of a tarball, following re-exports.
fromSource ::
  -- | What the plan settles about its conditionals.
  Macros ->
  -- | What the module's package puts in force, before its own pragmas.
  [Extension] ->
  -- | How to reach another module, for chasing re-exports. This is
  -- 'resolveModule' tied back on itself, with the visiting set already
  -- extended.
  (Text -> IO (Maybe (Fixities))) ->
  -- | How to reach another module for what its names carry with them.
  (Text -> IO (Map OpName (Set OpName))) ->
  -- | Modules currently being resolved, passed through so that a
  -- re-export chain cannot loop.
  Set Text ->
  -- | The tarball holding this module's source.
  FilePath ->
  -- | The module to read.
  Text ->
  -- | What it declares, including what it only reexports, and whether that
  -- is worth remembering.
  IO Reading
fromSource macros extensions reach reachChildren visiting tarball modName =
  doesFileExist tarball >>= \case
    False -> pure NoArchive
    True ->
      readModule tarball modName >>= \case
        Nothing -> pure (FromArchive (Unreadable Nothing))
        Just ForHsc -> pure (FromArchive (hscDeclares modName))
        Just (Haskell source) ->
          FromArchive
            <$> fromText macros extensions reach reachChildren visiting source modName

-- | What came of looking for a module in an archive.
data Reading
  = -- | The archive was there, and this is what reading it established.
    FromArchive Established
  | -- | There was no archive to open.
    NoArchive

-- | The fixities a module's text declares and passes on.
fromText ::
  -- | What the plan settles about its conditionals.
  Macros ->
  -- | What the module's package puts in force, before its own pragmas.
  [Extension] ->
  -- | How to reach another module, for chasing re-exports. This is
  -- 'resolveModule' tied back on itself, with the visiting set already
  -- extended.
  (Text -> IO (Maybe (Fixities))) ->
  -- | How to reach another module for what its names carry with them.
  (Text -> IO (Map OpName (Set OpName))) ->
  -- | Modules currently being resolved, passed through so that a re-export
  -- chain cannot loop.
  Set Text ->
  -- | The module's source.
  Text ->
  -- | Its name.
  Text ->
  IO Established
fromText macros extensions reach reachChildren visiting source modName =
  case configurationsOf macros (Just extensions) modName source of
    Nothing -> pure (Unreadable Nothing)
    Just modules ->
      agreeing <$> traverse readOne modules
  where
    readOne (implicitPrelude, hsModule) =
      withReexports
        implicitPrelude
        reach
        reachChildren
        visiting
        modName
        hsModule

-- | One answer from every configuration that could be read, if they agree.
--
-- A module may declare a fixity in one configuration and a different one in
-- another. Which of them holds depends on how the module is compiled, which
-- is not ours to decide, so disagreement is not an answer. Agreement across
-- the ones we could read is one, and a stronger one than the blanked text
-- could give.
--
-- A configuration whose imports could not be resolved is passed over rather
-- than counted against the rest, because almost every one of those is a
-- branch meant for a different platform.
agreeing :: NonEmpty Established -> Established
agreeing answers = case [fixities | Declares fixities <- toList answers] of
  [] -> Unreadable (listToMaybe (catMaybes [below | Unreadable below <- toList answers]))
  readable ->
    maybe (Unreadable Nothing) Declares (foldM together Map.empty readable)
  where
    together settled found
      | and (Map.intersectionWith (==) settled found) = Just (Map.union settled found)
      | otherwise = Nothing

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
      entries <- listDirectory dir
      case filter (".cabal" `isSuffixOf`) entries of
        [] -> pure Map.empty
        (cabalFile : _) -> do
          contents <- readFileText (dir </> cabalFile)
          case contents of
            Nothing -> pure Map.empty
            Just text ->
              Map.fromList . concat
                <$> traverse (locate dir (sourceDirs text)) (containedModules text)
    locate dir dirs m = do
      found <-
        filterM
          doesFileExist
          [ dir </> T.unpack d </> modulePath m ending
          | d <- dirs,
            ending <- moduleEndings
          ]
      pure [(m, path) | path <- take 1 found]

    modulePath m ending = T.unpack (T.replace "." "/" m) <> ending

-- | Read a file, if it is there and is text.
readFileText :: FilePath -> IO (Maybe Text)
readFileText path = quietly Nothing $ do
  there <- doesFileExist path
  if there
    then Just . T.decodeUtf8Lenient <$> BS.readFile path
    else pure Nothing

-- | What a module reexports, as well as what it declares.
withReexports ::
  -- | Whether @ImplicitPrelude@ is on in the module being read.
  Choice "implicitPrelude" ->
  -- | How to reach another module, for names this one only passes on.
  (Text -> IO (Maybe (Fixities))) ->
  -- | How to reach another module for what its names carry with them,
  -- which is what a @T(..)@ this module hands on amounts to.
  (Text -> IO (Map OpName (Set OpName))) ->
  -- | Modules currently being resolved. A candidate already in here is
  -- skipped rather than followed.
  Set Text ->
  -- | The name this module was looked up under, used to recognise a
  -- @module M@ export that refers to the module itself.
  Text ->
  -- | The module, already parsed.
  HsModule GhcPs ->
  -- | What it declares together with what it re-exports, or the module it
  -- passes names on from that could not be read.
  IO Established
withReexports implicitPrelude reach reachChildren visiting modName hsModule =
  case moduleExports hsModule of
    Nothing -> pure (Declares own)
    Just items -> do
      carried <- carriedNames implicitPrelude reachChildren hsModule items
      let wanted = wantedNames items <> fromCarried carried
      visible <-
        if null wanted
          then pure []
          else
            traverse
              (\i -> (,) i <$> fromModule (importModule i))
              (moduleImports implicitPrelude hsModule)
      let handedOnWhole =
            wantedModules implicitPrelude modName hsModule items
      wholeModules <- traverse (\m -> (,) m <$> fromModule m) handedOnWhole
      pure $ case stoppedAt visible wholeModules of
        Just below -> Unreadable (Just below)
        Nothing ->
          let seen = [(i, exported) | (i, Just exported) <- visible]
              whole = [exported | (_, Just exported) <- wholeModules]
              passedOn =
                Map.unions
                  [ found
                  | (qualifier, op) <- wanted,
                    found <- take 1 (from qualifier op seen)
                  ]
           in Declares (Map.unions (own : passedOn : whole))
  where
    stoppedAt visible wholeModules =
      listToMaybe $
        [importModule i | (i, Nothing) <- visible]
          <> [m | (m, Nothing) <- wholeModules]
    own = declaredFixities hsModule
    defined = declaredNames hsModule
    wantedNames items =
      [(qualifier, op) | ExportName qualifier op <- items, not (Set.member op defined)]
        <> [(qualifier, op) | ExportAll qualifier op <- items, not (Set.member op defined)]
    fromCarried carried =
      [ (qualifier, op)
      | ((qualifier, _), Just ops) <- carried,
        op <- Set.toList ops,
        not (Set.member op defined)
      ]
    from qualifier op seen =
      [ found
      | (i, exported) <- seen,
        canSupply qualifier op i,
        let found = Map.filterWithKey (\(_, o) _ -> o == op) exported,
        not (Map.null found)
      ]
    fromModule m
      | m `Set.member` visiting = pure (Just Map.empty)
      | otherwise = reach m

-- | What each name a module's export list hands on carries with it.
childrenWithReexports ::
  -- | Whether @ImplicitPrelude@ is on in the module being read.
  Choice "implicitPrelude" ->
  -- | How to reach another module for what its names carry.
  (Text -> IO (Map OpName (Set OpName))) ->
  -- | The name this module was looked up under.
  Text ->
  -- | The module, already parsed.
  HsModule GhcPs ->
  IO (Map OpName (Set OpName))
childrenWithReexports implicitPrelude reachChildren modName hsModule =
  case moduleExports hsModule of
    Nothing -> pure (moduleChildren hsModule)
    Just items -> do
      carried <- carriedNames implicitPrelude reachChildren hsModule items
      let handedOnWhole =
            wantedModules implicitPrelude modName hsModule items
      wholes <- traverse reachChildren handedOnWhole
      pure . Map.unionsWith Set.union $
        moduleChildren hsModule
          : Map.fromListWith Set.union [(parent, ops) | ((_, parent), Just ops) <- carried]
          : wholes

-- | The operators a module's export list names, following what it
-- reexports.
exportNamesWithReexports ::
  -- | Whether @ImplicitPrelude@ is on in the module being read.
  Choice "implicitPrelude" ->
  -- | How to reach another module for what its export list names.
  (Text -> IO (Maybe (Set OpName))) ->
  -- | How to reach another module for what its names carry.
  (Text -> IO (Map OpName (Set OpName))) ->
  -- | The name this module was looked up under.
  Text ->
  -- | The module, already parsed.
  HsModule GhcPs ->
  IO (Maybe (Set OpName))
exportNamesWithReexports
  implicitPrelude
  reachNames
  reachChildren
  modName
  hsModule =
    case moduleExports hsModule of
      Nothing -> pure (Just (Set.fromList [op | (_, op) <- Map.keys (declaredFixities hsModule)]))
      Just items -> do
        carried <- carriedNames implicitPrelude reachChildren hsModule items
        let handedOnWhole =
              wantedModules implicitPrelude modName hsModule items
        wholes <- traverse reachNames handedOnWhole
        pure $ do
          fromWholes <- sequence wholes
          fromCarried <-
            traverse (\((_, parent), kids) -> Set.insert parent <$> kids) carried
          pure (Set.unions (named items : declaredHere items : fromCarried <> fromWholes))
    where
      declared = declaredChildren hsModule
      named items = Set.fromList [op | ExportName _ op <- items]
      declaredHere items =
        Set.unions
          [ Set.insert parent kids
          | ExportAll _ parent <- items,
            Just kids <- [Map.lookup parent declared]
          ]

-- | What the types a module reexports carry with them, asked of the modules
-- they could have come from.
carriedNames ::
  -- | Whether @ImplicitPrelude@ is on in the module being read.
  Choice "implicitPrelude" ->
  -- | How to reach another module for what its export list names.
  (Text -> IO (Map OpName (Set OpName))) ->
  -- | The module, already parsed.
  HsModule GhcPs ->
  -- | Export items.
  [ExportItem] ->
  -- | For each reexported name, what it carries, or 'Nothing' where no
  -- module that could have supplied it had anything to say about it.
  IO [((Maybe Text, OpName), Maybe (Set OpName))]
carriedNames implicitPrelude reachChildren hsModule items =
  traverse (\(qualifier, parent) -> ((qualifier, parent),) <$> carriedBy qualifier parent) handedOn
  where
    declared = declaredChildren hsModule
    imports = moduleImports implicitPrelude hsModule
    handedOn =
      [ (qualifier, parent)
      | ExportAll qualifier parent <- items,
        not (Map.member parent declared)
      ]
    carriedBy qualifier parent = do
      answers <- traverse (reachChildren . importModule) (filter (canSupply qualifier parent) imports)
      pure $ case mapMaybe (Map.lookup parent) answers of
        [] -> Nothing
        kids -> Just (Set.unions kids)

-- | Could this import have supplied a name an export list reexports?
canSupply :: Maybe Text -> OpName -> Import -> Bool
canSupply qualifier op i =
  reaches && case importNames i of
    Nothing -> True
    Just (True, hidden) -> not (surelyNames Map.empty op hidden)
    Just (False, shown) -> mightBring Map.empty op shown
  where
    reaches = case qualifier of
      Nothing -> not (importQualified i)
      Just q -> importAlias i == q

-- | The modules a @module M@ export reexports whole, by their own names.
wantedModules ::
  -- | Whether @ImplicitPrelude@ is on in the module being read.
  Choice "implicitPrelude" ->
  -- | The name this module was looked up under, so that a module handing
  -- itself on under it is not chased.
  Text ->
  -- | The module, already parsed.
  HsModule GhcPs ->
  -- | Export items.
  [ExportItem] ->
  -- | The modules named, with an alias resolved to what it was imported
  -- as, and this module itself left out.
  [Text]
wantedModules implicitPrelude modName hsModule items =
  Set.toList . Set.fromList $
    concat [under m | ExportModule m <- items, not (isSelf m)]
  where
    under m = case [importModule i | i <- imports, importAlias i == m] of
      [] -> [m]
      aliased -> aliased
    imports = moduleImports implicitPrelude hsModule
    isSelf m = Just m == moduleName hsModule || m == modName

-- | Every configuration the preprocessor allows of a module's text that is
-- Haskell, parsed, each with whether it has the Prelude without importing
-- it.
configurationsOf ::
  -- | What the plan settles about the questions its conditionals ask.
  Macros ->
  -- | What extensions the module's package puts in force.
  Maybe [Extension] ->
  -- | The module's name, for the parser to put in its errors.
  Text ->
  -- | Its text.
  Text ->
  Maybe (NonEmpty (Choice "implicitPrelude", HsModule GhcPs))
configurationsOf macros extensions modName text =
  NE.nonEmpty . mapMaybe parsed
    =<< whatParsed (branchLeaves (withoutRuledOut macros text))
  where
    parsed leaf =
      (,) (hasImplicitPrelude (fromMaybe [] extensions) leaf) . pmModule
        <$> whatParsed (parseModule (configOf leaf) named leaf)
    whatParsed = either (const Nothing) Just
    configOf leaf = maybe defaultParserConfig (configFor leaf) extensions
    configFor leaf exts = parserConfigFor (effectiveExtensions exts leaf)
    named = T.unpack modName

-- | Does this module see the Prelude without importing it?
hasImplicitPrelude :: [Extension] -> Text -> Choice "implicitPrelude"
hasImplicitPrelude extensions source =
  fromBool (ImplicitPrelude `elem` effectiveExtensions extensions source)

-- | Find a module inside a tarball and say what was found.
readModule :: FilePath -> Text -> IO (Maybe InArchive)
readModule tarball modName = quietly Nothing $ do
  bytes <- BL.readFile tarball
  let (cabal, candidates) = sweep Nothing [] (Tar.read (GZip.decompress bytes))
      dirs = maybe [] sourceDirs cabal
  pure (listToMaybe (mapMaybe (pick dirs candidates) moduleEndings))
  where
    suffix ending = "/" <> T.unpack (T.replace "." "/" modName) <> ending
    suffixes = fmap suffix moduleEndings
    sweep cabal found = \case
      Tar.Next entry rest
        | Tar.NormalFile content _ <- Tar.entryContent entry,
          cabalFileAtTop (entryPosixPath entry),
          Nothing <- cabal ->
            sweep (Just (decode content)) found rest
        | Tar.NormalFile content _ <- Tar.entryContent entry,
          any (`isSuffixOf` entryPosixPath entry) suffixes ->
            sweep cabal ((entryPosixPath entry, decode content) : found) rest
        | otherwise -> sweep cabal found rest
      _ -> (cabal, reverse found)
    pick dirs candidates ending =
      inArchive ending . snd
        <$> listToMaybe (under sfx dirs matching <> matching)
      where
        sfx = suffix ending
        matching = [c | c <- candidates, sfx `isSuffixOf` fst c]
    under sfx dirs matching = [e | d <- dirs, e <- matching, inDir sfx d (fst e)]
    inDir sfx d path
      | d == "." = takeWhile (/= '/') path <> sfx == path
      | otherwise = ("/" <> T.unpack d <> sfx) `isSuffixOf` path
    decode = T.decodeUtf8Lenient . BL.toStrict

-- | The endings a package may write a module under, in the order they are
-- tried.
moduleEndings :: [String]
moduleEndings = [".hs", ".hsc"]

-- | What an archive holds for a module.
data InArchive
  = -- | Haskell, as the package wrote it.
    Haskell Text
  | -- | A module written for @hsc2hs@. Its text is not kept: there is
    -- nothing to be done with it, and 'hscFixities' answers for it.
    ForHsc

-- | What was found under one ending amounts to.
inArchive :: String -> Text -> InArchive
inArchive ending text
  | writtenForHsc ending = ForHsc
  | otherwise = Haskell text

-- | Is this a module @hsc2hs@ writes rather than one anybody compiles?
writtenForHsc :: FilePath -> Bool
writtenForHsc = isSuffixOf ".hsc"

-- | What an @.hsc@ module declares, which is nothing unless it is named.
hscDeclares :: Text -> Established
hscDeclares modName =
  Declares (maybe Map.empty inBothNamespaces (Map.lookup modName hscFixities))

-- | The fixities an 'Established' holds, where it holds any.
fixitiesEstablished :: Established -> Maybe (Fixities)
fixitiesEstablished = \case
  Declares fixities -> Just fixities
  Unreadable _ -> Nothing

-- | The operators an @.hsc@ module can supply, on the same reasoning.
hscSupplies :: Text -> Set OpName
hscSupplies modName =
  maybe Set.empty Map.keysSet (Map.lookup modName hscFixities)
