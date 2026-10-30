{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Information coming from @.cabal@ files.
module Tilia.Cabal.Package
  ( PackageProblem (..),
    describePackageProblem,
    readPackageDescription,
    parsePackageDescription,
    declaredModules,
    packageSourceDirs,
    packageExtensions,
    PackageReader,
    newPackageReader,
    ComponentSection (..),
    BranchSections,
    Kind (..),
    packageOf,
    packageComponents,
    packageBranches,
    addedSources,
    sourceExtensions,
    isDeclaredModule,
    joined,
    setupScript,
  )
where

import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Monad ((<=<))
import Data.ByteString qualified as BS
import Data.Either (rights)
import Data.Foldable (toList)
import Data.Generics.Schemes (listify)
import Data.IORef
import Data.List (nub, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Fields.ParseResult (runParseResult)
import Distribution.ModuleName qualified as ModuleName
import Distribution.PackageDescription
  ( Benchmark (..),
    BenchmarkInterface (..),
    BuildInfo (..),
    CondBranch (..),
    CondTree (..),
    ConfVar (..),
    Executable (..),
    ForeignLib (..),
    GenericPackageDescription (..),
    Library (..),
    PackageDescription (..),
    PackageFlag (..),
    TestSuite (..),
    TestSuiteInterface (..),
    pkgName,
    unPackageName,
    unUnqualComponentName,
  )
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec (PErrorWithSource (..), showPError, simpleParsec)
import Distribution.System (buildArch, buildOS)
import Distribution.Types.BuildInfo (usedExtensions)
import Distribution.Types.CondTree (simplifyCondTree)
import Distribution.Utils.Path (SymbolicPathX, getSymbolicPath)
import Distribution.Version (Version, withinRange)
import GHC.Driver.Session qualified as GHC
import GHC.LanguageExtensions.Type (Extension)
import Language.Haskell.Extension qualified as Cabal
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.FilePath
  ( dropExtension,
    equalFilePath,
    joinPath,
    normalise,
    splitDirectories,
    takeDirectory,
    takeExtension,
    (</>),
  )
import Tilia.Cabal.Project (cabalFilesIn, startingDirectory)
import Tilia.Pragma (Defaults, defaultsOf, lookupEdition, nameSwitch)
import Tilia.Process (readProgramOutput)
import Tilia.Utils (pathUnder, quietly, readBytes, tshow)

-- | Why a file's extensions could not be settled.
data PackageProblem
  = -- | No @.cabal@ file at or above the module.
    NoPackageFile
  | -- | A @.cabal@ file that could not be read at all, and what went wrong.
    PackageUnreadable FilePath Text
  | -- | A @.cabal@ file that was read but did not parse, and everything the
    -- parser had to say about it. One message per line, each already
    -- carrying the position it refers to.
    PackageMalformed FilePath [Text]
  | -- | A @.cabal@ file naming no component whose @hs-source-dirs@ holds
    -- the module.
    FileUnclaimed FilePath
  deriving (Eq, Show)

-- | Say what went wrong, in one line.
describePackageProblem :: PackageProblem -> Text
describePackageProblem = \case
  NoPackageFile -> "no .cabal file above it"
  PackageUnreadable file why -> T.pack file <> " could not be read: " <> why
  PackageMalformed file complaints ->
    T.pack file <> " does not parse:" <> foldMap ("\n  " <>) complaints
  FileUnclaimed file ->
    T.pack file <> " names no component whose hs-source-dirs holds it"

-- | What we retain from reading a .cabal file.
type PackageReader = FilePath -> IO (Either PackageProblem Defaults)

-- | A 'PackageReader' that remembers what it has already worked out.
newPackageReader ::
  -- | The compiler's version, where the build plan names it, which spares
  -- asking the compiler.
  Maybe Version ->
  -- | The @.cabal@ files of the project, which a module no @.cabal@ file
  -- above it claims is looked for in.
  [FilePath] ->
  IO PackageReader
newPackageReader planned project = do
  covering <- newIORef Map.empty
  described <- newIORef Map.empty
  known <- newMVar (Just <$> planned)
  let compiler = modifyMVar known $ \case
        Just version -> pure (Just version, version)
        Nothing -> do
          version <- compilerVersion
          pure (Just version, version)
      elsewhere file problem = do
        found <- traverse (componentBranchesOf compiler described) project
        pure $ case claiming file (concat (rights found)) of
          Just c -> Right (componentExtensions c)
          Nothing -> Left problem
  pure $ \path -> quietly (Left NoPackageFile) $ do
    file <- canonicalizePath path
    from <- startingDirectory file
    findCabalFile covering from >>= \case
      Nothing -> elsewhere file NoPackageFile
      Just cabalFile ->
        componentBranchesOf compiler described cabalFile >>= \case
          Left problem -> pure (Left problem)
          Right components
            | equalFilePath file (takeDirectory cabalFile </> setupScript) ->
                pure (Right (defaultsOf (GHC.languageExtensions Nothing) []))
            | Just c <- claiming file components ->
                pure (Right (componentExtensions c))
            | otherwise -> elsewhere file (FileUnclaimed cabalFile)

-- | One branch of a component, with everything in force in it already
-- worked out.
data ComponentBranch = ComponentBranch
  { -- | Its source directories, absolute and canonical.
    componentDirs :: [FilePath],
    -- | The modules this branch names, each as a path without an
    -- extension relative to whichever of its source directories holds it.
    componentModules :: Set FilePath,
    -- | Its entry points, relative to its source directories in the same
    -- way but with their extensions.
    componentEntries :: Set FilePath,
    -- | What it puts in force.
    componentExtensions :: Defaults
  }

-- | Which branch holds the file, of those whose directories cover it.
claiming :: FilePath -> [ComponentBranch] -> Maybe ComponentBranch
claiming file components =
  case sortOn (Down . fst) [((named c, nearness c), c) | c <- components, covered c] of
    ((_, c) : _) -> Just c
    [] -> Nothing
  where
    covered = not . null . covering
    named c = any (declares c) (covering c)
    declares c (_, under) =
      isDeclaredModule (componentModules c) (componentEntries c) (joinPath under)
    nearness = maximum . fmap (length . fst) . covering
    covering c =
      [ (d, under)
      | d <- componentDirs c,
        Just under@(_ : _) <- [pathUnder d file]
      ]

-- | Is a file, named relative to a source directory, one of the modules or
-- entry points declared there?
isDeclaredModule ::
  -- | The modules, without their extensions.
  Set FilePath ->
  -- | The entry points.
  Set FilePath ->
  FilePath ->
  Bool
isDeclaredModule modules entries path =
  takeExtension path `elem` sourceExtensions
    && (Set.member path entries || Set.member (dropExtension path) modules)

-- | The nearest @.cabal@ file at or above a directory.
findCabalFile ::
  -- | What is known already, by directory: the file covering it, or
  -- 'Nothing' for one with no @.cabal@ anywhere above it. Read before the
  -- walk and added to after it, for every directory the walk passed through
  -- rather than only the one asked about—none of the others held a @.cabal@
  -- either, which is why the walk went through them, so the answer is
  -- theirs as well.
  IORef (Map FilePath (Maybe FilePath)) ->
  -- | Where to start, which is walked upwards until a @.cabal@ file turns
  -- up or the filesystem root is reached.
  FilePath ->
  IO (Maybe FilePath)
findCabalFile ref = climb []
  where
    climb passed directory = do
      known <- readIORef ref
      case Map.lookup directory known of
        Just answer -> settle passed answer
        Nothing -> do
          cabalFilesIn directory >>= \case
            (found : _) -> settle (directory : passed) (Just found)
            [] ->
              let parent = takeDirectory directory
               in if parent == directory
                    then settle (directory : passed) Nothing
                    else climb (directory : passed) parent
    settle passed answer = do
      modifyIORef' ref (\m -> foldl' (\acc d -> Map.insert d answer acc) m passed)
      pure answer

-- | What a @.cabal@ file amounts to.
componentBranchesOf ::
  IO (Maybe Version) ->
  IORef (Map FilePath (Either PackageProblem [ComponentBranch])) ->
  FilePath ->
  IO (Either PackageProblem [ComponentBranch])
componentBranchesOf compiler ref cabalFile = do
  known <- readIORef ref
  case Map.lookup cabalFile known of
    Just answer -> pure answer
    Nothing -> do
      answer <- settle
      modifyIORef' ref (Map.insert cabalFile answer)
      pure answer
  where
    settle =
      readPackageDescription cabalFile >>= \case
        Left problem -> pure (Left problem)
        Right description -> do
          valueOf <- conditionValues compiler description
          Right
            <$> traverse
              (componentBranch (takeDirectory cabalFile))
              (sectionsInForce valueOf description)

-- | Read and parse a @.cabal@ file, or say why it cannot be.
readPackageDescription :: FilePath -> IO (Either PackageProblem GenericPackageDescription)
readPackageDescription cabalFile =
  readBytes cabalFile >>= \case
    Left why -> pure (Left (PackageUnreadable cabalFile why))
    Right bytes -> pure (parsePackageDescription cabalFile bytes)

-- | Parse what a @.cabal@ file holds, or say why it cannot be.
parsePackageDescription ::
  -- | Where it comes from, named in what goes wrong.
  FilePath ->
  BS.ByteString ->
  Either PackageProblem GenericPackageDescription
parsePackageDescription cabalFile bytes =
  case snd (runParseResult (parseGenericPackageDescription bytes)) of
    Left (_, complaints) ->
      Left (PackageMalformed cabalFile (fmap said (NE.toList complaints)))
    Right description -> Right description
  where
    said = T.pack . showPError cabalFile . perror

-- | The value a build of a package on this platform gives each variable its
-- conditions ask about.
conditionValues ::
  -- | The compiler's version, asked for only where a condition names it.
  IO (Maybe Version) ->
  -- | The package, whose flags take their defaults.
  GenericPackageDescription ->
  IO (ConfVar -> Either ConfVar Bool)
conditionValues compiler described = do
  version <-
    if null (listify namesCompiler described)
      then pure Nothing
      else compiler
  pure $ \case
    OS os -> Right (os == buildOS)
    Arch arch -> Right (arch == buildArch)
    v@(PackageFlag name) -> maybe (Left v) Right (lookup name defaults)
    v@(Impl GHC range) -> maybe (Left v) (Right . (`withinRange` range)) version
    Impl _ _ -> Right False
  where
    namesCompiler = \case
      Impl _ _ -> True
      _ -> False
    defaults = [(flagName f, flagDefault f) | f <- genPackageFlags described]

-- | The version of the @ghc@ on the path, which builds the package.
compilerVersion :: IO (Maybe Version)
compilerVersion =
  (simpleParsec . T.unpack . T.strip =<<)
    <$> readProgramOutput "ghc" ["--numeric-version"]

-- | One branch, with its directories resolved and its extensions settled.
componentBranch :: FilePath -> ComponentSection -> IO ComponentBranch
componentBranch root ComponentSection{..} = do
  dirs <- traverse (quietlyCanonical . (root </>)) (sourceDirsOf sectionInfo)
  pure
    ComponentBranch
      { componentDirs = concat dirs,
        componentModules = Set.fromList (fmap (uncurry joined) sectionModules),
        componentEntries = Set.fromList (fmap (uncurry joined) sectionEntries),
        componentExtensions = extensionsInForce sectionInfo
      }
  where
    quietlyCanonical d =
      quietly [] $
        doesDirectoryExist d >>= \case
          True -> pure <$> canonicalizePath d
          False -> pure []

-- | What is in force in each branch of every component, in the order they
-- are declared.
sectionsInForce ::
  -- | The value the build gives each variable a condition asks about.
  (ConfVar -> Either ConfVar Bool) ->
  -- | The package.
  GenericPackageDescription ->
  [ComponentSection]
sectionsInForce valueOf described =
  ( \BranchSections{..} ->
      inheritedSection
        <> ownSection
        <> ComponentSection (builtSettings valueOf) [] []
  )
    <$> packageBranches described

-- | Every branch of every component of a package, in the order declared.
packageBranches :: GenericPackageDescription -> [BranchSections]
packageBranches described =
  [b | (_, _, branches) <- packageComponents described, b <- branches]

-- | The kinds of component a @.cabal@ file can declare, and the setup
-- script beside it.
data Kind = Lib | FLib | Exe | Test | Bench | Setup
  deriving (Eq, Ord, Show)

-- | Every component of a package, in the order declared: its kind, its
-- name, which for the main library is the package's own, and its branches.
packageComponents :: GenericPackageDescription -> [(Kind, Text, [BranchSections])]
packageComponents described =
  concat
    [ [(Lib, packageOf described, libraryBranches t) | t <- toList (condLibrary described)],
      [(Lib, nameOf n, libraryBranches t) | (n, t) <- condSubLibraries described],
      [(FLib, nameOf n, foreignLibBranches t) | (n, t) <- condForeignLibs described],
      [(Exe, nameOf n, executableBranches t) | (n, t) <- condExecutables described],
      [(Test, nameOf n, suiteBranches t) | (n, t) <- condTestSuites described],
      [(Bench, nameOf n, benchmarkBranches t) | (n, t) <- condBenchmarks described]
    ]
  where
    nameOf = T.pack . unUnqualComponentName

-- | The name of a package.
packageOf :: GenericPackageDescription -> Text
packageOf =
  T.pack . unPackageName . pkgName . package . packageDescription

-- | Each source directory the branches given name, with every module they
-- declare there and where it would be written under it, without an
-- extension.
declaredModules :: [BranchSections] -> [(FilePath, [(Text, FilePath)])]
declaredModules branches =
  [ ( d,
      [ ( T.intercalate "." (fmap T.pack (splitDirectories sub <> [name])),
          joined sub name
        )
      | (sub, name) <- sectionModules section
      ]
    )
  | branch <- branches,
    (d, section) <- addedSources branch
  ]

-- | Every directory any branch of a package names for its files, and then
-- the package's own.
packageSourceDirs :: GenericPackageDescription -> [FilePath]
packageSourceDirs described = nub (named <> ["."])
  where
    named =
      [ getSymbolicPath d
      | branch <- packageBranches described,
        d <- hsSourceDirs (sectionInfo (ownSection branch))
      ]

-- | What any branch of a package puts in force, all together: every edition
-- it names and every extension it turns on or off, in order.
packageExtensions :: GenericPackageDescription -> Defaults
packageExtensions described =
  defaultsOf
    baseline
    (mapMaybe extensionSwitch (concatMap usedExtensions infos))
  where
    infos =
      [sectionInfo (ownSection branch) | branch <- packageBranches described]
    baseline = case mapMaybe (ghcLanguage <=< defaultLanguage) infos of
      [] -> GHC.languageExtensions (Just GHC.Haskell98)
      editions -> nub (concatMap (GHC.languageExtensions . Just) editions)

-- | What one branch of a component declares.
data ComponentSection = ComponentSection
  { -- | Its build settings.
    sectionInfo :: BuildInfo,
    -- | Its modules, each as the directory that holds it relative to a
    -- source directory and its file name without an extension.
    sectionModules :: [(FilePath, FilePath)],
    -- | Its entry points, each as the directory that holds it relative to a
    -- source directory and its file name.
    sectionEntries :: [(FilePath, FilePath)]
  }

instance Semigroup ComponentSection where
  ComponentSection i m e <> ComponentSection i' m' e' =
    ComponentSection (i <> i') (m <> m') (e <> e')

instance Monoid ComponentSection where
  mempty = ComponentSection mempty [] []

-- | The sections of one branch of a component.
data BranchSections = BranchSections
  { -- | What it inherits from the branches around it.
    inheritedSection :: ComponentSection,
    -- | What it declares itself.
    ownSection :: ComponentSection,
    -- | The build settings of its component in a build, given the value the
    -- build gives each variable a condition asks about.
    builtSettings :: (ConfVar -> Either ConfVar Bool) -> BuildInfo
  }

-- | Every branch of a library.
libraryBranches :: CondTree ConfVar Library -> [BranchSections]
libraryBranches = branchesOf $ \l ->
  declaring (libBuildInfo l) (exposedModules l <> signatures l) []

-- | Every branch of a foreign library.
foreignLibBranches :: CondTree ConfVar ForeignLib -> [BranchSections]
foreignLibBranches = branchesOf $ \f -> declaring (foreignLibBuildInfo f) [] []

-- | Every branch of an executable.
executableBranches :: CondTree ConfVar Executable -> [BranchSections]
executableBranches = branchesOf $ \e ->
  declaring (buildInfo e) [] [entryPoint (modulePath e)]

-- | Every branch of a test suite.
suiteBranches :: CondTree ConfVar TestSuite -> [BranchSections]
suiteBranches = branchesOf $ \s ->
  case testInterface s of
    TestSuiteExeV10 _ path -> declaring (testBuildInfo s) [] [entryPoint path]
    TestSuiteLibV09 _ modName -> declaring (testBuildInfo s) [modName] []
    _ -> declaring (testBuildInfo s) [] []

-- | Every branch of a benchmark.
benchmarkBranches :: CondTree ConfVar Benchmark -> [BranchSections]
benchmarkBranches = branchesOf $ \b ->
  case benchmarkInterface b of
    BenchmarkExeV10 _ path -> declaring (benchmarkBuildInfo b) [] [entryPoint path]
    _ -> declaring (benchmarkBuildInfo b) [] []

-- | Every branch of a component.
branchesOf ::
  (Semigroup a) =>
  (a -> ComponentSection) ->
  CondTree ConfVar a ->
  [BranchSections]
branchesOf f root = go mempty root
  where
    built valueOf =
      sectionInfo (f (simplifyCondTree valueOf root))
    go inherited node =
      let own = f (condTreeData node)
       in BranchSections inherited own built
            : concatMap (branches (inherited <> own)) (condTreeComponents node)
    branches inherited (CondBranch _ yes no) =
      go inherited yes <> foldMap (go inherited) no

-- | Where the files a branch adds to its component may be: each source
-- directory, with what it may hold.
--
-- Only what the branches around it have not already paired, which is its
-- own declarations in every directory in force and what it inherits in the
-- directories it adds. A pair made for one branch is thus never made again
-- for the branches inside it.
addedSources :: BranchSections -> [(FilePath, ComponentSection)]
addedSources BranchSections{..} =
  [ (d, ownSection)
  | d <- sourceDirsOf (sectionInfo inheritedSection <> sectionInfo ownSection)
  ]
    <> [ (d, inheritedSection)
       | d <- fmap getSymbolicPath (hsSourceDirs (sectionInfo ownSection))
       ]

-- | What a component's own section declares, modules together with the
-- ones its build settings name.
declaring ::
  BuildInfo ->
  [ModuleName.ModuleName] ->
  [FilePath] ->
  ComponentSection
declaring bi ms entries =
  ComponentSection
    { sectionInfo = bi,
      sectionModules = fmap split (ms <> otherModules bi),
      sectionEntries = fmap splitPath entries
    }
  where
    split m = case ModuleName.components m of
      [] -> ("", "")
      cs -> (joinPath (init cs), last cs)
    splitPath path = case splitDirectories path of
      [] -> ("", "")
      ps -> (joinPath (init ps), last ps)

-- | The extensions a Haskell source file can have.
sourceExtensions :: [String]
sourceExtensions = [".hs", ".hs-boot", ".hsig"]

-- | Two relative paths one after the other, where either may be empty or
-- @.@.
joined :: FilePath -> FilePath -> FilePath
joined a b
  | a `elem` ["", "."] = b
  | b `elem` ["", "."] = a
  | otherwise = a </> b

-- | The path of an entry point.
entryPoint :: SymbolicPathX allowAbsolute from to -> FilePath
entryPoint = normalise . getSymbolicPath

-- | The setup script of a package, beside its @.cabal@ file.
--
-- @cabal@ compiles it with no language or extensions of its own, whatever
-- the components around it put in force.
setupScript :: FilePath
setupScript = "Setup.hs"

-- | The @hs-source-dirs@ of a component, @.@ where it names none.
sourceDirsOf :: BuildInfo -> [FilePath]
sourceDirsOf bi = case fmap getSymbolicPath (hsSourceDirs bi) of
  [] -> ["."]
  ds -> ds

-- | What a component puts in force, before any module's pragmas.
extensionsInForce :: BuildInfo -> Defaults
extensionsInForce bi =
  defaultsOf
    (GHC.languageExtensions edition)
    (mapMaybe extensionSwitch (usedExtensions bi))
  where
    edition = ghcLanguage (fromMaybe Cabal.Haskell98 (defaultLanguage bi))

-- | GHC's reading of an extension Cabal turns on or off, where GHC knows it.
extensionSwitch :: Cabal.Extension -> Maybe (GHC.OnOff Extension)
extensionSwitch = \case
  Cabal.EnableExtension e -> nameSwitch (tshow e)
  Cabal.DisableExtension e -> nameSwitch ("No" <> tshow e)
  _ -> Nothing

-- | GHC's name for a language edition, when it has one.
ghcLanguage :: Cabal.Language -> Maybe GHC.Language
ghcLanguage = lookupEdition . tshow
