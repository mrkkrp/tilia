{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

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
    readGivenPlan,
    fetchUninstalled,
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
import Control.Concurrent
  ( MVar,
    ThreadId,
    getNumCapabilities,
    modifyMVar,
    modifyMVar_,
    myThreadId,
    newEmptyMVar,
    newMVar,
    readMVar,
    tryPutMVar,
    tryReadMVar,
  )
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.DeepSeq (NFData, force)
import Control.Exception (bracket_, evaluate, onException)
import Control.Monad (filterM, join, void)
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
import Data.Choice (Choice, fromBool, isTrue, pattern Do)
import Data.Foldable (toList, traverse_)
import Data.IORef
import Data.List (isSuffixOf)
import Data.List qualified
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Read qualified as T
import Data.Unique (Unique, newUnique)
import GHC.Generics (Generic)
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
import Tilia.Cabal.Package (newPackageReader)
import Tilia.Cpp.Directives (branchLeaves, withoutRuledOut)
import Tilia.Cpp.Macros (Macros (..))
import Tilia.Fixity
import Tilia.Fixity.ByHand (hscFixities)
import Tilia.Fixity.Cabal
  ( cabalFileAtTop,
    containedModules,
    declaredExtensions,
    entryPosixPath,
    packageModules,
    sourceDirs,
  )
import Tilia.Fixity.Cache
import Tilia.Fixity.HiFile (primopFixities)
import Tilia.Fixity.Interface
import Tilia.Fixity.PackageDb
import Tilia.Parser
import Tilia.Pragma (effectiveExtensions)
import Tilia.Process (readProgramOutput)
import Tilia.Utils (inParallel, quietly)

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
    . T.decodeUtf8Lenient
    . B16.encode
    . SHA256.hash
    . T.encodeUtf8
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
  mempty
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
          ],
      macroUndefined =
        if null compiler
          then Set.empty
          else Set.fromList ["__MHS__", "__HUGS__"]
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
checkReadiness :: Choice "useCache" -> [PlanComponent] -> FilePath -> IO Readiness
checkReadiness caching wanted projectDir =
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
          sourcesShortOf caching plan >>= \case
            [] -> pure Ready
            ps -> pure (SourcesMissing (fmap ppName ps))

-- | The packages the plan expects to fetch whose sources are not here.
sourcesShortOf :: Choice "useCache" -> BuildPlan -> IO [PlanPackage]
sourcesShortOf caching plan = do
  tarballs <- filter (isFetchable . fst) <$> plannedTarballs plan
  absent <- fmap fst <$> filterM (fmap not . doesFileExist . snd) tarballs
  short <-
    if null absent
      then pure []
      else do
        cache <- openCache caching =<< tokenForBuildPlan plan
        installed <- getInstalledPackages cache
        pure (filter (not . builtAlready installed) absent)
  pure short

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
futilityFor :: Choice "useCache" -> FilePath -> Futility
futilityFor caching projectDir =
  Futility
    { solveWasFutile = withCache False cachedFutileSolve,
      rememberFutileSolve = withCache () storeFutileSolve,
      fetchWasFutileFor = withCache [] cachedFutileFetch,
      rememberFutileFetch = \packages ->
        withCache () (`storeFutileFetch` packages)
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
    fetch
      | isTrue downloading = tryWholeProject ["build", ":all", "--only-download"]
      | otherwise = pure (Right ())
    fetchWhatIsShort =
      readBuildPlan (planPathFor projectDir) >>= \case
        Left _ -> pure (Right ())
        Right plan -> do
          short <- fmap ppName <$> sourcesShortOf caching plan
          refused <- fetchWasFutileFor futility
          if null short || all (`elem` refused) short
            then pure (Right ())
            else
              fetch >>= \case
                Left err -> pure (Left err)
                Right () -> do
                  left <- sourcesShortOf caching plan
                  rememberFutileFetch futility (fmap ppName left)
                  pure (Right ())
    solveThenFetch =
      tryWholeProject ["build", ":all", "--dry-run"] >>= \case
        Left err -> pure (Left err)
        Right () ->
          checkReadiness caching wanted projectDir >>= \case
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
fetchUninstalled caching projectDir plan =
  sourcesShortOf caching plan >>= \case
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
  readiness <- checkReadiness caching wanted projectDir
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
newResolver = newResolverVia (Do #useCache) [FromInterface, FromSource]

-- | 'newResolver', restricted to the routes given.
newResolverVia ::
  -- | Whether to use the cache.
  Choice "useCache" ->
  -- | Which readings to try, in order.
  [Route] ->
  -- | The build plan to use.
  BuildPlan ->
  IO Resolver
newResolverVia caching routes plan = do
  tarballs <-
    if FromSource `elem` routes
      then plannedTarballs plan
      else pure []
  cache <- openCache caching =<< tokenForBuildPlan plan
  installed <- getInstalledPackages cache
  index <- buildModuleIndex cache installed tarballs
  let interfaces = interfaceIndex installed
  local <- localModules plan
  flights <- newMVar (Flights Map.empty Map.empty)
  answersRead <- newMemo
  askPackage <- newPackageReader
  extensionsRead <- newMemo
  summariesRead <- newMemo
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
        memoized flights summariesRead Nothing modName $ do
          extensions <- extensionsOf modName
          let summarized =
                evaluate . force $
                  configurationsOf
                    (macrosOf plan)
                    (Just extensions)
                    modName
                    text
              stamp =
                digestOf $
                  T.intercalate "\n" [T.pack (show extensions), macrosRead, text]
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
            either (const []) id <$> askPackage path
        | Just (package, tarball) <- Map.lookup modName index =
            memoized flights extensionsRead [] package $
              foldMap declaredExtensions . (archiveCabal =<<) <$> archiveOf tarball
        | otherwise = pure []
      archiveOf tarball =
        memoized flights archivesRead Nothing (T.pack tarball) (readArchive tarball)
      moduleInArchive tarball modName = (moduleIn modName =<<) <$> archiveOf tarball
  pure Resolver{askModule = reach Set.empty}

-- | Answers filed under the names they are about, each worked out once.
data Memo v = Memo Unique (IORef (Map Text (MVar v)))

-- | An empty 'Memo'.
newMemo :: IO (Memo v)
newMemo = Memo <$> newUnique <*> newIORef Map.empty

-- | Which thread is working out which answer, and which answer each waiting
-- thread waits for, across all of a resolver's tables.
data Flights = Flights
  { flightOwners :: Map (Unique, Text) ThreadId,
    flightWaits :: Map ThreadId (Unique, Text)
  }

-- | Look an answer up in a table, working it out and filing it the first
-- time.
memoized ::
  -- | Who is working out what.
  MVar Flights ->
  -- | Where the answers are filed.
  Memo v ->
  -- | The answer for a thread that cannot wait.
  v ->
  -- | What the answer is about.
  Text ->
  -- | Work that is being memoized.
  IO v ->
  IO v
memoized flights (Memo table answers) standIn key work =
  Map.lookup key <$> readIORef answers >>= \case
    Just slot -> tryReadMVar slot >>= maybe (waitFor slot) pure
    Nothing -> do
      me <- myThreadId
      claimed <- modifyMVar flights $ \fl -> do
        filed <- readIORef answers
        case Map.lookup key filed of
          Just slot -> pure (fl, Left slot)
          Nothing -> do
            slot <- newEmptyMVar
            writeIORef answers (Map.insert key slot filed)
            pure
              ( fl{flightOwners = Map.insert (table, key) me (flightOwners fl)},
                Right slot
              )
      case claimed of
        Left slot -> waitFor slot
        Right slot -> do
          answer <- work `onException` file slot standIn
          answer <$ file slot answer
  where
    file slot answer = modifyMVar_ flights $ \fl -> do
      _ <- tryPutMVar slot answer
      pure fl{flightOwners = Map.delete (table, key) (flightOwners fl)}
    waitFor slot = do
      me <- myThreadId
      settled <- modifyMVar flights $ \fl ->
        tryReadMVar slot >>= \case
          Just answer -> pure (fl, Just answer)
          Nothing
            | waitedOnBy fl me (table, key) -> pure (fl, Just standIn)
            | otherwise ->
                pure
                  ( fl{flightWaits = Map.insert me (table, key) (flightWaits fl)},
                    Nothing
                  )
      case settled of
        Just answer -> pure answer
        Nothing -> do
          answer <- readMVar slot
          modifyMVar_ flights $ \fl ->
            pure fl{flightWaits = Map.delete me (flightWaits fl)}
          pure answer

-- | Is this answer being worked out by the given thread, or by one that
-- waits for it, directly or through others?
waitedOnBy :: Flights -> ThreadId -> (Unique, Text) -> Bool
waitedOnBy fl me = go
  where
    go key = case Map.lookup key (flightOwners fl) of
      Nothing -> False
      Just owner
        | owner == me -> True
        | otherwise -> maybe False go (Map.lookup owner (flightWaits fl))

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
-- 'newResolverVia' builds it once and hands it over whole. What comes back
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
    -- 'newResolverVia', so that the memo it keeps covers the recursive
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
            fromSummaries (wkReach visiting') modName =<< wkSummariesOf modName source
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
        | Set.member modName wkGenerated = settledAs Map.empty established
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
    keyFor dir = "interface-" <> digestOf (T.pack dir)

-- | A name for some text, short enough to file something under.
digestOf :: Text -> Text
digestOf =
  T.take 24 . T.decodeUtf8Lenient . B16.encode . SHA256.hash . T.encodeUtf8

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
  -- | The modules it exposes, or 'Nothing' if the tarball is absent, fails
  -- verification, or holds no @.cabal@ file.
  IO (Maybe [Text])
fromCabalFile cache key tarball p =
  recalled (cachedModules cache key) (storeModules cache key) $
    verified p tarball >>= \case
      False -> pure Nothing
      True -> packageModules tarball

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
      pure . together $
        mempty
          { establishedFixities = summaryFixities summary,
            establishedMembers = summaryListedMembers summary
          }
          : fmap (exported answers reexported) items
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

-- | An answer whose fixities come from elsewhere, and settle every name.
settledAs :: Fixities -> Established -> Established
settledAs fixities established =
  established
    { establishedFixities = fixities,
      establishedUnsettled = Map.empty,
      establishedUntold = Set.empty
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
  Maybe [Extension] ->
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
      summarize (hasImplicitPrelude (fromMaybe [] extensions) leaf) . pmModule
        <$> whatParsed (parseModule (configOf leaf) named leaf)
    whatParsed = either (const Nothing) Just
    configOf leaf = maybe defaultParserConfig (configFor leaf) extensions
    configFor leaf exts = parserConfigFor (effectiveExtensions exts leaf)
    named = T.unpack modName

-- | Does this module see the Prelude without importing it?
hasImplicitPrelude :: [Extension] -> Text -> Choice "implicitPrelude"
hasImplicitPrelude extensions source =
  fromBool (ImplicitPrelude `elem` effectiveExtensions extensions source)

-- | What a tarball holds that a module could be read out of.
data Archive = Archive
  { -- | The package's @.cabal@ file, the first one at the top.
    archiveCabal :: Maybe Text,
    -- | Every file that could hold a module, by where it sits, in the order
    -- the tarball has them.
    archiveFiles :: [(FilePath, BS.ByteString)]
  }
  deriving (Generic)

instance NFData Archive

-- | Read a tarball, keeping what a module could be read out of.
readArchive :: FilePath -> IO (Maybe Archive)
readArchive tarball = quietly Nothing $ do
  bytes <- BL.readFile tarball
  Just <$> evaluate (force (sweep Nothing [] (Tar.read (GZip.decompress bytes))))
  where
    sweep cabal found = \case
      Tar.Next entry rest
        | Tar.NormalFile content _ <- Tar.entryContent entry,
          cabalFileAtTop (entryPosixPath entry),
          Nothing <- cabal ->
            sweep (Just (T.decodeUtf8Lenient (BL.toStrict content))) found rest
        | Tar.NormalFile content _ <- Tar.entryContent entry,
          any (`isSuffixOf` entryPosixPath entry) moduleEndings ->
            sweep cabal ((entryPosixPath entry, BL.toStrict content) : found) rest
        | otherwise -> sweep cabal found rest
      _ -> Archive cabal (reverse found)

-- | Find a module in what a tarball holds and say what was found.
moduleIn :: Text -> Archive -> Maybe InArchive
moduleIn modName archive = listToMaybe (mapMaybe pick moduleEndings)
  where
    dirs = maybe [] sourceDirs (archiveCabal archive)
    pick ending =
      inArchive ending . T.decodeUtf8Lenient . snd
        <$> listToMaybe (under sfx matching <> matching)
      where
        sfx = "/" <> T.unpack (T.replace "." "/" modName) <> ending
        matching = [f | f <- archiveFiles archive, sfx `isSuffixOf` fst f]
    under sfx matching = [f | d <- dirs, f <- matching, inDir sfx d (fst f)]
    inDir sfx d path
      | d == "." = takeWhile (/= '/') path <> sfx == path
      | otherwise = ("/" <> T.unpack d <> sfx) `isSuffixOf` path

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
  mempty
    { establishedFixities =
        maybe Map.empty inBothNamespaces (Map.lookup modName hscFixities)
    }
