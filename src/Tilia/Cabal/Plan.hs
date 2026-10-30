{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A project's build plan, and the @cabal@ runs that make it ready to read.
module Tilia.Cabal.Plan
  ( -- * Build plans
    PlanPackage (..),
    PackageSource (..),
    isFetchable,
    sourceHashOf,
    cacheKey,
    BuildPlan (..),
    readBuildPlan,
    readGivenPlan,
    fetchUninstalled,
    getInstalledPackages,
    tokenForEnvAndBuildPlan,
    tokenForBuildPlan,
    macrosOf,
    versionMacros,
    plannedCompiler,
    generatedModules,

    -- * Readiness
    Readiness (..),
    PlanComponent (..),
    plannedComponents,
    planPathFor,
    checkReadiness,
    plannedTarballs,
    packageCacheRoot,
    guessedPackageCacheRoot,
    Futility (..),
    undiscoveredFutility,
    futilityFor,
    prepareWith,
    loadPlan,
  )
where

import Control.Monad (filterM, join, void)
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
import Data.Choice (Choice, isFalse)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Foldable (toList, traverse_)
import Data.List qualified
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Read qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Distribution.Package (pkgName, pkgVersion, unPackageName)
import Distribution.PackageDescription (package, packageDescription)
import Distribution.Pretty (prettyShow)
import Distribution.Version (Version, mkVersion)
import GHC.IO.Handle (hDuplicate)
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
import System.FilePath (isRelative, takeDirectory, (</>))
import System.IO (hClose, hFlush, stderr)
import System.Info qualified
import System.Process
  ( StdStream (CreatePipe, Inherit, UseHandle),
    createProcess,
    cwd,
    proc,
    std_err,
    std_in,
    std_out,
    waitForProcess,
  )
import Tilia.Cabal.Package (readPackageDescription)
import Tilia.Cabal.Project (cabalFilesIn, projectConfigFiles)
import Tilia.Cpp.Macros (Macros (..))
import Tilia.Fixity.Cache
import Tilia.Fixity.PackageDb
import Tilia.Process (readProgramOutputIn)
import Tilia.Utils (quietly, sha256Of)

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
getInstalledPackages :: Cache -> IO [InstalledPackage]
getInstalledPackages cache =
  cachedInstalled cache >>= \case
    Just packages -> pure packages
    Nothing -> do
      found <- readInstalledPackages
      storeInstalled cache found
      pure (installedPackages found)

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
    . sha256Of
    $ T.intercalate
      "\n"
      ( environment
          : bpCompiler plan
          : Data.List.sort (fmap cacheKey (bpPackages plan))
      )

-- | The SHA-256 the plan expects this package's tarball to have.
sourceHashOf :: PlanPackage -> Maybe Text
sourceHashOf p = case ppSource p of
  RepoPackage hash _ -> hash
  _ -> Nothing

-- | How a package's cached answers are filed.
cacheKey :: PlanPackage -> Text
cacheKey p =
  ppName p <> "-" <> ppVersion p <> maybe "" (("-" <>) . T.take 16) (sourceHashOf p)

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
                ( case ( join (join repoKind) :: Maybe Text,
                         join (join repoPath),
                         join (join repoUri)
                       ) of
                    (Just "local-repo-no-index", Just dir, _) ->
                      RepoFromDirectory (T.unpack dir)
                    (_, _, Just uri) -> RepoDownloaded uri
                    _ -> RepoNoProvenance
                )
            (_, Just "local") -> LocalPackage (maybe "" T.unpack (join sourcePath))
            (_, Just "source-repo") -> SourceRepo
            -- Anything else, a local or remote tarball among them, is
            -- treated as already present.
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
  decodePlan path >>= traverse (checkedOutIn (takeDirectory (takeDirectory path)))

-- | Read a plan that is to be trusted as up to date.
readGivenPlan ::
  -- | The project root.
  FilePath ->
  -- | The plan.
  FilePath ->
  IO (Either Text BuildPlan)
readGivenPlan root path =
  fmap (\plan -> plan{bpPackages = fmap rooted (bpPackages plan)})
    <$> decodePlan path
  where
    rooted p = case ppSource p of
      LocalPackage dir
        | isRelative dir ->
            p{ppSource = LocalPackage (root </> dir)}
      _ -> p

-- | Decode a plan, or say why it cannot be.
decodePlan :: FilePath -> IO (Either Text BuildPlan)
decodePlan path =
  doesFileExist path >>= \case
    False -> pure (Left ("no build plan at " <> T.pack path))
    True -> either (Left . T.pack) Right <$> eitherDecodeFileStrict path

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
    isThePackage p dir =
      either (const False) (describes p . package . packageDescription)
        <$> readPackageDescription (dir </> T.unpack (ppName p) <> ".cabal")
    describes p identifier =
      T.pack (unPackageName (pkgName identifier)) == ppName p
        && T.pack (prettyShow (pkgVersion identifier)) == ppVersion p

-- | The version macros a plan settles.
macrosOf :: BuildPlan -> Macros
macrosOf plan =
  ( versionMacros
      (numberedVersion =<< T.stripPrefix "ghc-" (bpCompiler plan))
      (Map.mapMaybe sole versions)
  )
    { macroPlannedPackages =
        Just (Set.fromList [underscored (ppName p) | p <- bpPackages plan])
    }
  where
    versions =
      Map.fromListWith
        Set.union
        [ (ppName p, Set.singleton v)
        | p <- bpPackages plan,
          Just v <- [numberedVersion (ppVersion p)]
        ]
    sole vs = case Set.toList vs of
      [v] -> Just v
      _ -> Nothing

-- | The version macros the compiler's version and each package's settle.
versionMacros ::
  -- | The compiler's version.
  Maybe [Integer] ->
  -- | Each package's version, by name.
  Map Text [Integer] ->
  Macros
versionMacros given versions =
  mempty
    { macroExactVersions =
        Map.fromList
          ( [ ("MIN_VERSION_" <> underscored name, version)
            | (name, version) <- Map.toList versions
            ]
              <> [("MIN_VERSION_GLASGOW_HASKELL", v) | v <- toList compiler]
          ),
      macroExactNumbers =
        Map.fromList
          [ entry
          | (major : minor : patches) <- toList compiler,
            entry <-
              [ ("__GLASGOW_HASKELL__", major * 100 + minor),
                ("__GLASGOW_HASKELL_PATCHLEVEL1__", nth 0 patches),
                ("__GLASGOW_HASKELL_PATCHLEVEL2__", nth 1 patches)
              ]
          ],
      macroUndefined =
        if null compiler
          then Set.empty
          else Set.fromList ["__MHS__", "__HUGS__"]
    }
  where
    compiler = case given of
      Just parts@(_ : _ : _) -> Just (take 4 (parts <> repeat 0))
      _ -> Nothing
    nth i xs = if i < length xs then xs !! i else 0

-- | The version of GHC a plan is for, where it is for GHC.
plannedCompiler :: BuildPlan -> Maybe Version
plannedCompiler plan =
  mkVersion . fmap fromInteger
    <$> (numberedVersion =<< T.stripPrefix "ghc-" (bpCompiler plan))

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
spellPlanComponent :: PlanComponent -> Text
spellPlanComponent c = pcPackage c <> ":" <> pcName c

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
  | -- | The plan is there, and the packages named have neither been
    -- downloaded nor built.
    SourcesMissing [Text]
  deriving (Eq, Show)

-- | Where @cabal@ writes the plan for a project.
planPathFor :: FilePath -> FilePath
planPathFor projectDir = projectDir </> "dist-newstyle" </> "cache" </> "plan.json"

-- | Check what is missing.
checkReadiness :: Choice "useCache" -> [PlanComponent] -> FilePath -> IO Readiness
checkReadiness caching wanted projectDir =
  readinessOf caching wanted projectDir =<< readBuildPlan (planPathFor projectDir)

-- | 'checkReadiness', given the plan as read.
readinessOf ::
  -- | Whether to use the cache.
  Choice "useCache" ->
  -- | The components the run is about to format.
  [PlanComponent] ->
  -- | The project whose plan it is.
  FilePath ->
  -- | Its plan, or why it could not be read.
  Either Text BuildPlan ->
  IO Readiness
readinessOf caching wanted projectDir = \case
  Left _ -> pure PlanMissing
  Right plan -> do
    cache <- openCache caching =<< tokenForBuildPlan plan
    solved <- cachedSolveTime cache
    newer <- filesNewerThanPlan plan projectDir solved
    let covered = plannedComponents plan
        missing = [spellPlanComponent c | c <- wanted, c `notElem` covered]
    case (newer, missing) of
      (_ : _, _) -> pure (PlanStale newer)
      ([], _ : _) -> pure (PlanNarrow missing)
      ([], []) ->
        sourcesShortOf cache projectDir plan >>= \case
          [] -> pure Ready
          ps -> pure (SourcesMissing (fmap ppName ps))

-- | The packages the plan expects to fetch whose sources are not here.
sourcesShortOf :: Cache -> FilePath -> BuildPlan -> IO [PlanPackage]
sourcesShortOf cache projectDir plan = do
  tarballs <-
    filter (isFetchable . fst) <$> plannedTarballs cache projectDir plan
  absent <- fmap fst <$> filterM (fmap not . doesFileExist . snd) tarballs
  if null absent
    then pure []
    else do
      installed <- getInstalledPackages cache
      pure (filter (not . builtAlready installed) absent)

-- | Has the compiler got this package already?
builtAlready :: [InstalledPackage] -> PlanPackage -> Bool
builtAlready installed p = any matches installed
  where
    matches i = ipName i == ppName p && ipVersion i == ppVersion p

-- | The project files that have changed since the plan was written, and
-- since the last solve, which leaves a plan it would not change as it was.
filesNewerThanPlan :: BuildPlan -> FilePath -> Maybe UTCTime -> IO [FilePath]
filesNewerThanPlan plan projectDir solved = quietly [] $ do
  written <- getModificationTime (planPathFor projectDir)
  let planTime = maybe written (max written) solved
  configs <- traverse (projectConfigFiles . (projectDir </>)) projectFiles
  atRootPackage <- cabalFilesIn projectDir
  inPackages <- concat <$> traverse cabalFilesIn (localDirs plan)
  let candidates = fmap fst (concat configs) <> atRootPackage <> inPackages
  newer <- traverse (isNewerThan planTime) candidates
  pure [f | Just f <- newer]
  where
    projectFiles =
      ["cabal.project", "cabal.project.local", "cabal.project.freeze"]
    localDirs p =
      Data.List.nub [dir | LocalPackage dir <- fmap ppSource (bpPackages p)]
    isNewerThan planTime path = quietly Nothing $ do
      t <- getModificationTime path
      pure (if t > planTime then Just path else Nothing)

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
    rememberFutileFetch :: [Text] -> IO (),
    -- | Remember when a solve started, so that no change made before it
    -- makes the plan stale.
    rememberSolveTime :: UTCTime -> IO ()
  }

-- | The state when we know nothing about futile actions yet.
undiscoveredFutility :: Futility
undiscoveredFutility =
  Futility
    { solveWasFutile = pure False,
      rememberFutileSolve = pure (),
      fetchWasFutileFor = pure [],
      rememberFutileFetch = const (pure ()),
      rememberSolveTime = const (pure ())
    }

-- | A memory kept in the cache, under the plan the project has now.
futilityFor :: Choice "useCache" -> FilePath -> Futility
futilityFor caching projectDir =
  Futility
    { solveWasFutile = withCache False cachedFutileSolve,
      rememberFutileSolve = withCache () storeFutileSolve,
      fetchWasFutileFor = withCache [] cachedFutileFetch,
      rememberFutileFetch = \packages ->
        withCache () (`storeFutileFetch` packages),
      rememberSolveTime = \started -> withCache () (`storeSolveTime` started)
    }
  where
    withCache fallback use =
      readBuildPlan (planPathFor projectDir) >>= \case
        Left _ -> pure fallback
        Right plan -> use =<< openCache caching =<< tokenForBuildPlan plan

-- | Do whatever is missing, given a way to run @cabal@ and a memory of what
-- earlier attempts came to.
prepareWith ::
  -- | Whether to use the cache.
  Choice "useCache" ->
  -- | Whether to download what is missing.
  Choice "download" ->
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
prepareWith caching downloading cabal futility wanted projectDir = \case
  Ready -> pure (Right ())
  SourcesMissing short -> fetch short
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
    fetch short
      | isFalse downloading = pure (Right ())
      | otherwise = do
          refused <- fetchWasFutileFor futility
          if all (`elem` refused) short
            then pure (Right ())
            else
              tryWholeProject ["build", ":all", "--only-download"] >>= \case
                Left err -> pure (Left err)
                Right () ->
                  withPlan ((Right () <$) . rememberFutileFetch futility)
    fetchWhatIsShort = withPlan fetch
    withPlan use =
      readBuildPlan (planPathFor projectDir) >>= \case
        Left _ -> pure (Right ())
        Right plan -> do
          cache <- openCache caching =<< tokenForBuildPlan plan
          use . fmap ppName =<< sourcesShortOf cache projectDir plan
    solveThenFetch = do
      started <- getCurrentTime
      tryWholeProject ["build", ":all", "--dry-run"] >>= \case
        Left err -> pure (Left err)
        Right () -> do
          rememberSolveTime futility started
          checkReadiness caching wanted projectDir >>= \case
            SourcesMissing short -> fetch short
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
  -- A pipe closed at once rather than our standard input, which in an
  -- editor's process carries what the editor says to it.
  (toChild, _, _, running) <-
    createProcess
      (proc "cabal" args)
        { cwd = Just projectDir,
          std_in = CreatePipe,
          std_out = UseHandle passed,
          std_err = Inherit
        }
  traverse_ hClose toChild
  code <- waitForProcess running
  pure $ case code of
    ExitSuccess -> Right ()
    _ -> Left ("cabal " <> T.unwords (fmap T.pack args) <> " failed; see above")

-- | Fetch the sources of a trusted plan's dependencies that are neither
-- installed nor fetched already, at the versions the plan names.
fetchUninstalled ::
  -- | Whether to use the cache.
  Choice "useCache" ->
  -- | The project the plan is for.
  FilePath ->
  -- | The build plan to use.
  BuildPlan ->
  IO ()
fetchUninstalled caching projectDir plan = do
  cache <- openCache caching =<< tokenForBuildPlan plan
  sourcesShortOf cache projectDir plan >>= \case
    [] -> pure ()
    short ->
      void . runCabal projectDir $
        "fetch"
          : "--no-dependencies"
          : [T.unpack (ppName p <> "-" <> ppVersion p) | p <- short]

-- | Get a plan that is safe to use, doing whatever @cabal@ work is needed.
loadPlan ::
  -- | Whether to use the cache.
  Choice "useCache" ->
  -- | Whether to download what is missing.
  Choice "download" ->
  -- | The components the run is about to format, so that a plan which says
  -- nothing about them is solved again rather than trusted.
  [PlanComponent] ->
  -- | The project whose plan it is.
  FilePath ->
  IO (Either Text BuildPlan)
loadPlan caching downloading wanted projectDir = do
  written <- readBuildPlan (planPathFor projectDir)
  readiness <- readinessOf caching wanted projectDir written
  case (readiness, written) of
    (Ready, Right plan) -> pure (Right plan)
    _ ->
      prepareWith
        caching
        downloading
        (runCabal projectDir)
        (futilityFor caching projectDir)
        wanted
        projectDir
        readiness
        >>= \case
          Left err -> pure (Left err)
          _ -> readBuildPlan (planPathFor projectDir)

-- | Every planned package whose source could be in the package cache, with
-- where that would be: under the package cache @cabal@ last named for the
-- project, unless a tarball it downloads is not there and a fetch has not
-- already failed to bring it in, when @cabal@ is asked again.
plannedTarballs ::
  -- | Where what @cabal@ said is remembered.
  Cache ->
  -- | The project the plan is for.
  FilePath ->
  -- | The plan.
  BuildPlan ->
  IO [(PlanPackage, FilePath)]
plannedTarballs cache projectDir plan =
  cachedPackageCache cache projectDir >>= \case
    Just remembered -> do
      tarballs <- under remembered
      refused <- cachedFutileFetch cache
      absent <-
        filterM
          (fmap not . doesFileExist . snd)
          [t | t@(p, _) <- tarballs, isFetchable p, ppName p `notElem` refused]
      if null absent then pure tarballs else asked
    Nothing -> asked
  where
    asked = do
      cacheRoot <- packageCacheRoot projectDir
      storePackageCache cache projectDir cacheRoot
      under cacheRoot
    under cacheRoot = do
      repos <- quietly [] (Data.List.sort <$> listDirectory cacheRoot)
      traverse
        (\p -> (,) p <$> tarballFor cacheRoot repos p)
        (nubOrdOn (\p -> (ppName p, ppVersion p)) [p | p <- bpPackages plan, not (isLocal p)])
    isLocal p = case ppSource p of
      LocalPackage _ -> True
      CheckedOut _ -> True
      _ -> False

-- | Where @cabal@ keeps the package sources it downloads for the project in
-- the directory given, one directory per repository it downloads from.
packageCacheRoot :: FilePath -> IO FilePath
packageCacheRoot projectDir =
  readProgramOutputIn
    projectDir
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
