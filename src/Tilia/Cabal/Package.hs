{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Information coming from @.cabal@ files.
module Tilia.Cabal.Package
  ( PackageProblem (..),
    describePackageProblem,
    PackageReader,
    newPackageReader,
  )
where

import Data.ByteString qualified as BS
import Data.IORef
import Data.List (isSuffixOf, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Fields.ParseResult (runParseResult)
import Distribution.ModuleName qualified as ModuleName
import Distribution.PackageDescription
  ( Benchmark (..),
    BenchmarkInterface (..),
    BuildInfo (..),
    CondTree (..),
    Executable (..),
    GenericPackageDescription (..),
    Library (..),
    TestSuite (..),
    TestSuiteInterface (..),
  )
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec (showPError)
import Distribution.Utils.Path (getSymbolicPath)
import GHC.Driver.Session qualified as GHC
import GHC.LanguageExtensions.Type (Extension)
import Language.Haskell.Extension qualified as Cabal
import System.Directory (canonicalizePath, doesDirectoryExist, listDirectory)
import System.FilePath
  ( equalFilePath,
    joinPath,
    splitDirectories,
    takeDirectory,
    (<.>),
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
          Right components -> pure $ case claiming file components of
            Just c -> Right (componentExtensions c)
            Nothing -> Left (FileUnclaimed cabalFile)

-- | Where to start looking for a @.cabal@ file.
startingDirectory :: FilePath -> IO FilePath
startingDirectory path = do
  isDirectory <- quietly False (doesDirectoryExist path)
  pure (if isDirectory then path else takeDirectory path)

-- | A component of a package, with everything about it already worked out.
data Component = Component
  { -- | Its source directories, absolute and canonical.
    componentDirs :: [FilePath],
    -- | The files this component names, each relative to whichever of its
    -- source directories holds it. Directory overlap alone does not make a
    -- unit test a module of the doctest driver beside it.
    componentFiles :: [FilePath],
    -- | The extensions it puts in force.
    componentExtensions :: [Extension]
  }

-- | Which component holds the file, of those whose directories cover it.
claiming :: FilePath -> [Component] -> Maybe Component
claiming file components =
  case sortOn (Down . fst) [((named c, nearness c), c) | c <- components, covered c] of
    ((_, c) : _) -> Just c
    [] -> Nothing
  where
    covered = not . null . covering
    named c = any (declares c) (covering c)
    declares c d = any (equalFilePath (under d)) (componentFiles c)
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
  IORef (Map FilePath (Either PackageProblem [Component])) ->
  FilePath ->
  IO (Either PackageProblem [Component])
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
                  (component (takeDirectory cabalFile))
                  (buildInfos description)
    said = T.pack . showPError cabalFile

-- | One component, with its directories resolved and its extensions settled.
component :: FilePath -> (BuildInfo, [FilePath]) -> IO Component
component root (bi, files) = do
  dirs <- traverse (quietlyCanonical . (root </>)) (sourceDirsOf bi)
  pure
    Component
      { componentDirs = concat dirs,
        componentFiles = files,
        componentExtensions = extensionsInForce bi
      }
  where
    sourceDirsOf b = case fmap getSymbolicPath (hsSourceDirs b) of
      [] -> ["."]
      ds -> ds
    quietlyCanonical d =
      quietly [] $
        doesDirectoryExist d >>= \case
          True -> pure <$> canonicalizePath d
          False -> pure []

-- | Every component's build settings, in the order they are declared.
buildInfos :: GenericPackageDescription -> [(BuildInfo, [FilePath])]
buildInfos described =
  concat
    [ foldMap (pure . library . condTreeData) (condLibrary described),
      named library (condSubLibraries described),
      named executable (condExecutables described),
      named suite (condTestSuites described),
      named benchmark (condBenchmarks described)
    ]
  where
    named f = fmap (f . condTreeData . snd)
    modules = concatMap (\m -> [ModuleName.toFilePath m <.> ext | ext <- ["hs", "hs-boot", "hsig"]])
    with bi files = (bi, files <> modules (otherModules bi))
    library l = with (libBuildInfo l) (modules (exposedModules l))
    executable e = with (buildInfo e) [entryPoint (modulePath e)]
    suite s = with (testBuildInfo s) $ case testInterface s of
      TestSuiteExeV10 _ path -> [entryPoint path]
      TestSuiteLibV09 _ modName -> modules [modName]
      _ -> []
    benchmark b = with (benchmarkBuildInfo b) $ case benchmarkInterface b of
      BenchmarkExeV10 _ path -> [entryPoint path]
      _ -> []
    -- Cabal 3.14 made a component's entry point a symbolic path; before
    -- that it was already the plain path we want.
#if MIN_VERSION_Cabal_syntax(3, 14, 0)
    entryPoint = getSymbolicPath
#else
    entryPoint = id
#endif

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
