{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Information coming from @.cabal@ files.
module Tilia.Cabal.Package
  ( PackageProblem (..),
    describePackageProblem,
    PackageReader,
    newPackageReader,
    ComponentSection (..),
    BranchSections,
    libraryBranches,
    executableBranches,
    suiteBranches,
    benchmarkBranches,
    addedSources,
    sourceExtensions,
    joined,
    setupScript,
  )
where

import Data.ByteString qualified as BS
import Data.IORef
import Data.List (isSuffixOf, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Fields.ParseResult (runParseResult)
import Distribution.ModuleName qualified as ModuleName
import Distribution.PackageDescription
  ( Benchmark (..),
    BenchmarkInterface (..),
    BuildInfo (..),
    CondBranch (..),
    CondTree (..),
    Executable (..),
    GenericPackageDescription (..),
    Library (..),
    TestSuite (..),
    TestSuiteInterface (..),
  )
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec (showPError)
#if MIN_VERSION_Cabal_syntax(3, 14, 0)
import Distribution.Utils.Path (SymbolicPathX, getSymbolicPath)
#else
import Distribution.Utils.Path (getSymbolicPath)
#endif
import GHC.Driver.Session qualified as GHC
import GHC.LanguageExtensions.Type (Extension)
import Language.Haskell.Extension qualified as Cabal
import System.Directory (canonicalizePath, doesDirectoryExist, listDirectory)
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
import Tilia.Pragma (lookupExtension)
import Tilia.Utils (attempted, quietly)

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
type PackageReader = FilePath -> IO (Either PackageProblem [Extension])

-- | A 'PackageReader' that remembers what it has already worked out.
newPackageReader :: IO PackageReader
newPackageReader = do
  covering <- newIORef Map.empty
  described <- newIORef Map.empty
  pure $ \path -> quietly (Left NoPackageFile) $ do
    file <- canonicalizePath path
    from <- startingDirectory file
    findCabalFile covering from >>= \case
      Nothing -> pure (Left NoPackageFile)
      Just cabalFile ->
        componentsOf described cabalFile >>= \case
          Left problem -> pure (Left problem)
          Right components
            | equalFilePath file (takeDirectory cabalFile </> setupScript) ->
                pure (Right (extensionsInForce mempty))
            | otherwise -> pure $ case claiming file components of
                Just c -> Right (componentExtensions c)
                Nothing -> Left (FileUnclaimed cabalFile)

-- | Where to start looking for a @.cabal@ file.
startingDirectory :: FilePath -> IO FilePath
startingDirectory path = do
  isDirectory <- quietly False (doesDirectoryExist path)
  pure (if isDirectory then path else takeDirectory path)

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
    -- | The extensions it puts in force.
    componentExtensions :: [Extension]
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
    declares c d =
      let path = under d
       in Set.member path (componentEntries c)
            || ( takeExtension path `elem` sourceExtensions
                   && Set.member (dropExtension path) (componentModules c)
               )
    under d = joinPath (drop (length (splitDirectories d)) (splitDirectories file))
    nearness = maximum . fmap length . covering
    covering c = [d | d <- componentDirs c, d `covers` file]

-- | Whether a file is somewhere under a directory.
covers :: FilePath -> FilePath -> Bool
covers directory file = go (splitDirectories directory) (splitDirectories file)
  where
    go [] (_ : _) = True
    go (d : ds) (f : fs) = equalFilePath d f && go ds fs
    go _ _ = False

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
          entries <- quietly [] (listDirectory directory)
          case filter (".cabal" `isSuffixOf`) entries of
            (named : _) -> settle (directory : passed) (Just (directory </> named))
            [] ->
              let parent = takeDirectory directory
               in if parent == directory
                    then settle (directory : passed) Nothing
                    else climb (directory : passed) parent
    settle passed answer = do
      modifyIORef' ref (\m -> foldl' (\acc d -> Map.insert d answer acc) m passed)
      pure answer

-- | What a @.cabal@ file amounts to.
componentsOf ::
  IORef (Map FilePath (Either PackageProblem [ComponentBranch])) ->
  FilePath ->
  IO (Either PackageProblem [ComponentBranch])
componentsOf ref cabalFile = do
  known <- readIORef ref
  case Map.lookup cabalFile known of
    Just answer -> pure answer
    Nothing -> do
      answer <- settle
      modifyIORef' ref (Map.insert cabalFile answer)
      pure answer
  where
    settle =
      attempted (BS.readFile cabalFile) >>= \case
        Left why -> pure (Left (PackageUnreadable cabalFile why))
        Right bytes ->
          case snd (runParseResult (parseGenericPackageDescription bytes)) of
            Left (_, complaints) ->
              pure (Left (PackageMalformed cabalFile (fmap said (NE.toList complaints))))
            Right description ->
              Right
                <$> traverse
                  (componentBranch (takeDirectory cabalFile))
                  (sectionsInForce description)
    said = T.pack . showPError cabalFile

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
sectionsInForce :: GenericPackageDescription -> [ComponentSection]
sectionsInForce described =
  (\BranchSections{..} -> inheritedSection <> ownSection)
    <$> concat
      [ foldMap libraryBranches (condLibrary described),
        concatMap (libraryBranches . snd) (condSubLibraries described),
        concatMap (executableBranches . snd) (condExecutables described),
        concatMap (suiteBranches . snd) (condTestSuites described),
        concatMap (benchmarkBranches . snd) (condBenchmarks described)
      ]

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
    ownSection :: ComponentSection
  }

-- | Every branch of a library.
libraryBranches :: CondTree v c Library -> [BranchSections]
libraryBranches = branchesOf $ \l ->
  declaring (libBuildInfo l) (exposedModules l <> signatures l) []

-- | Every branch of an executable.
executableBranches :: CondTree v c Executable -> [BranchSections]
executableBranches = branchesOf $ \e ->
  declaring (buildInfo e) [] [entryPoint (modulePath e)]

-- | Every branch of a test suite.
suiteBranches :: CondTree v c TestSuite -> [BranchSections]
suiteBranches = branchesOf $ \s ->
  case testInterface s of
    TestSuiteExeV10 _ path -> declaring (testBuildInfo s) [] [entryPoint path]
    TestSuiteLibV09 _ modName -> declaring (testBuildInfo s) [modName] []
    _ -> declaring (testBuildInfo s) [] []

-- | Every branch of a benchmark.
benchmarkBranches :: CondTree v c Benchmark -> [BranchSections]
benchmarkBranches = branchesOf $ \b ->
  case benchmarkInterface b of
    BenchmarkExeV10 _ path -> declaring (benchmarkBuildInfo b) [] [entryPoint path]
    _ -> declaring (benchmarkBuildInfo b) [] []

-- | Every branch of a component.
branchesOf :: (a -> ComponentSection) -> CondTree v c a -> [BranchSections]
branchesOf f = go mempty
  where
    go inherited node =
      let own = f (condTreeData node)
       in BranchSections inherited own
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
--
-- Cabal 3.14 made a component's entry point a symbolic path; before that
-- it was already the plain path we want.
#if MIN_VERSION_Cabal_syntax(3, 14, 0)
entryPoint :: SymbolicPathX allowAbsolute from to -> FilePath
entryPoint = normalise . getSymbolicPath
#else
entryPoint :: FilePath -> FilePath
entryPoint = normalise
#endif

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

-- | The extensions a component puts in force, before any module's pragmas.
extensionsInForce :: BuildInfo -> [Extension]
extensionsInForce bi =
  foldl apply (GHC.languageExtensions edition) (defaultExtensions bi)
  where
    edition = ghcLanguage =<< defaultLanguage bi
    apply acc = \case
      Cabal.EnableExtension e
        | Just on <- named e, on `notElem` acc -> acc <> [on]
      Cabal.DisableExtension e
        | Just off <- named e -> filter (/= off) acc
      _ -> acc
    named = lookupExtension . T.pack . show

-- | GHC's name for a language edition, when it has one.
ghcLanguage :: Cabal.Language -> Maybe GHC.Language
ghcLanguage l = lookup (show l) [(show e, e) | e <- [minBound .. maxBound]]
