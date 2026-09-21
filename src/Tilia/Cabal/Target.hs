{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Handling Cabal targets in order to figure out what to format.
module Tilia.Cabal.Target
  ( Target (..),
    Kind (..),
    parseTarget,
    Component (..),
    TargetProblem (..),
    describeTargetProblem,
    componentsOfTarget,
    componentInPlan,
    filesOfComponents,
  )
where

import Control.Monad (filterM)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (toLower)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Distribution.Fields.Field (Field (..), FieldLine (..), Name (..))
import Distribution.Fields.ParseResult (runParseResult)
import Distribution.Fields.Parser (readFields)
import Distribution.PackageDescription
  ( Benchmark (..),
    BuildInfo (..),
    CondTree (..),
    Executable (..),
    GenericPackageDescription (..),
    Library (..),
    PackageDescription (..),
    TestSuite (..),
    unPackageName,
    unUnqualComponentName,
  )
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec (showPError)
import Distribution.Types.PackageId (PackageIdentifier (..))
import Distribution.Utils.Path (getSymbolicPath)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath
  ( normalise,
    splitDirectories,
    takeDirectory,
    takeExtension,
    (</>),
  )
import Tilia.Cabal.Project (Marker (..), ProjectRoot (..), markerFile)
import Tilia.Fixity.Plan (PlanComponent (..))
import Tilia.Utils (attempted, quietly)

-- | Which components a run was asked for.
data Target
  = -- | @all@, or nothing given at all: every component of every package.
    Everything
  | -- | One bare word, which may name a package or a component.
    Called Text
  | -- | @kind:name@, or @package:kind:name@.
    Qualified (Maybe Text) Kind Text
  deriving (Eq, Show)

-- | The kinds of component a @.cabal@ file can declare.
data Kind = Lib | Exe | Test | Bench
  deriving (Eq, Ord, Show)

-- | Read a target as it was written on the command line.
parseTarget :: String -> Either Text Target
parseTarget written = case T.splitOn ":" (T.strip (T.pack written)) of
  [""] -> Left "an empty target"
  ["all"] -> Right Everything
  [one] -> Right (Called one)
  [k, name] | Just kind <- kindNamed k -> Right (Qualified Nothing kind name)
  [package, k, name] | Just kind <- kindNamed k -> Right (Qualified (Just package) kind name)
  _ -> Left unrecognised
  where
    unrecognised =
      "unrecognised target "
        <> T.pack (show written)
        <> ": expected all, a package or component name, or one of\
           \ lib:, exe:, test:, bench: followed by a name"
    kindNamed = \case
      "lib" -> Just Lib
      "exe" -> Just Exe
      "test" -> Just Test
      "bench" -> Just Bench
      "benchmark" -> Just Bench
      _ -> Nothing

-- | Does a target ask for this component?
targetSelectsComponent :: Target -> Component -> Bool
targetSelectsComponent target c = case target of
  Everything -> True
  Called name -> name == componentName c || name == componentPackage c
  Qualified package kind name ->
    all (== componentPackage c) package
      && kind == componentKind c
      && name == componentName c

-- | One component of one package, as far as formatting cares.
data Component = Component
  { -- | The package it belongs to.
    componentPackage :: Text,
    -- | Which kind it is.
    componentKind :: Kind,
    -- | Its name, which for a library is the package's own.
    componentName :: Text,
    -- | The directory its @.cabal@ file sits in.
    componentRoot :: FilePath,
    -- | Its @hs-source-dirs@, relative to 'componentRoot'.
    componentDirs :: [FilePath]
  }
  deriving (Eq, Show)

-- | Why a run could not work out what to format.
data TargetProblem
  = -- | A @cabal.project@ naming packages, none of which could be found.
    NoPackages FilePath
  | -- | A @.cabal@ file that would not parse, and what the parser said.
    Unparseable FilePath [Text]
  | -- | A target naming no component the project holds.
    NoSuchTarget Text [Text]
  deriving (Eq, Show)

-- | Say what went wrong.
describeTargetProblem :: TargetProblem -> Text
describeTargetProblem = \case
  NoPackages file ->
    T.pack file <> " names no packages that exist"
  Unparseable file complaints ->
    T.pack file <> " does not parse:" <> foldMap ("\n  " <>) complaints
  NoSuchTarget asked available ->
    "no component matches "
      <> asked
      <> ", and the targets this project takes are"
      <> foldMap ("\n  " <>) ("all" : available)

-- | Every component of the project that the target asks for.
componentsOfTarget ::
  ProjectRoot ->
  Target ->
  IO (Either TargetProblem [Component])
componentsOfTarget root target = do
  files <- packageFilesOf root
  if null files
    then pure (Left (NoPackages (prPath root </> markerFile (prMarker root))))
    else
      traverse componentsInCabalFile files >>= \case
        results
          | (problem : _) <- [p | Left p <- results] -> pure (Left problem)
          | otherwise -> do
              let found = concat [cs | Right cs <- results]
              pure $ case filter (targetSelectsComponent target) found of
                [] | Everything <- target -> Right []
                [] -> Left (NoSuchTarget (spellTarget target) (fmap spellComponent found))
                wanted -> Right wanted

-- | How a component would have to be named to be asked for on its own.
spellComponent :: Component -> Text
spellComponent c =
  componentPackage c
    <> ":"
    <> spellKind (componentKind c)
    <> ":"
    <> componentName c

-- | How a build plan names this component.
--
-- A plan writes a library as @lib@ and everything else as its kind and
-- name, which is not quite how a target is written: see 'spellComponent'.
componentInPlan :: Component -> PlanComponent
componentInPlan c =
  PlanComponent
    { pcPackage = componentPackage c,
      pcName = case componentKind c of
        Lib -> "lib"
        kind -> spellKind kind <> ":" <> componentName c
    }

-- | Render 'Kind' the way it would be accepted on the command line.
spellKind :: Kind -> Text
spellKind = \case
  Lib -> "lib"
  Exe -> "exe"
  Test -> "test"
  Bench -> "bench"

-- | A target, written the way it would have been given.
spellTarget :: Target -> Text
spellTarget = \case
  Everything -> "all"
  Called name -> name
  Qualified package kind name ->
    T.intercalate ":" (foldMap pure package <> [spellKind kind, name])

-- | Every Haskell file a set of components holds, each named once, less
-- the ones the project's @.tiliaignore@ excludes.
filesOfComponents :: ProjectRoot -> [Component] -> IO [FilePath]
filesOfComponents root components = do
  ignored <- ignoredPaths (prPath root)
  sort . Set.toList . Set.fromList . concat
    <$> traverse (filesOfComponent ignored) components

-- | Every Haskell file in a component that is not excluded, in a settled
-- order.
filesOfComponent ::
  -- | The excluded paths, as 'ignoredPaths' gives them.
  [[FilePath]] ->
  -- | The component whose source directories to walk.
  Component ->
  IO [FilePath]
filesOfComponent ignored c =
  sort . Set.toList . Set.fromList . concat
    <$> traverse walk (filter (not . excluded) sourceDirs)
  where
    sourceDirs = fmap (normalise . (componentRoot c </>)) (componentDirs c)
    walk directory =
      quietly [] $
        doesDirectoryExist directory >>= \case
          False -> pure []
          True -> do
            entries <- listDirectory directory
            concat <$> traverse (below directory) (sort entries)
    below directory entry
      | "." `isPrefixOf` entry = pure []
      | entry == "dist-newstyle" = pure []
      | excluded path = pure []
      | otherwise = do
          isDirectory <- quietly False (doesDirectoryExist path)
          if isDirectory
            then walk path
            else pure [path | takeExtension path `elem` formattableFileExtensions]
      where
        path = directory </> entry
    excluded path = any (`isPrefixOf` splitDirectories path) ignored

-- | What a project's @.tiliaignore@ excludes, each path as its segments.
--
-- An entry is a literal file or directory path relative to the project
-- root, and a directory stands for everything below it. Blank lines and
-- lines beginning with @#@ are ignored.
ignoredPaths :: FilePath -> IO [[FilePath]]
ignoredPaths root = do
  exists <- doesFileExist file
  if exists
    then fmap entryPath . filter meant . fmap T.strip . T.lines <$> T.readFile file
    else pure []
  where
    file = root </> ".tiliaignore"
    meant entry = not (T.null entry) && not ("#" `T.isPrefixOf` entry)
    entryPath = splitDirectories . normalise . (root </>) . T.unpack

-- | The extensions a Haskell source file can have.
formattableFileExtensions :: [String]
formattableFileExtensions = [".hs", ".hs-boot", ".hsig"]

-- | The @.cabal@ files the project is made of.
--
-- A @cabal.project@ names them, possibly through globs; anything else means
-- the marker found by the walk upwards is itself the only package.
packageFilesOf :: ProjectRoot -> IO [FilePath]
packageFilesOf root = case prMarker root of
  PackageFile named -> pure [prPath root </> named]
  ProjectFile -> do
    contents <-
      quietly BS.empty (BS.readFile (prPath root </> "cabal.project"))
    found <-
      traverse
        (packageToCabalFile (prPath root))
        (packagesInCabalProjectContents contents)
    pure (Set.toList (Set.fromList (concat found)))

-- | The entries of a @cabal.project@'s @packages@ field.
packagesInCabalProjectContents :: BS.ByteString -> [Text]
packagesInCabalProjectContents contents = case readFields contents of
  Left _ -> []
  Right fields -> concatMap entries (packagesIn fields)
  where
    packagesIn = concatMap $ \case
      Field (Name _ name) ls
        | BS8.map toLower name `elem` ["packages", "optional-packages"] ->
            [T.unwords [T.decodeUtf8Lenient value | FieldLine _ value <- ls]]
        | otherwise -> []
      Section _ _ inner -> packagesIn inner
    entries =
      filter (not . T.null)
        . fmap T.strip
        . concatMap (T.split (== ','))
        . T.words

-- | Turn one entry of a @packages@ field into the @.cabal@ files it names.
packageToCabalFile :: FilePath -> Text -> IO [FilePath]
packageToCabalFile root entry = do
  paths <-
    packageGlobToCabalFiles
      root
      (fmap T.unpack (T.split (== '/') (T.dropWhile (== '.') stripped)))
  concat <$> traverse asPackage paths
  where
    stripped = T.dropWhile (== '/') (T.strip entry)
    asPackage path
      | ".cabal" `isSuffixOf` path = do
          there <- quietly False (doesFileExist path)
          pure [path | there]
      | otherwise = cabalFilesIn path

-- | Resolve a path whose components may contain @*@.
packageGlobToCabalFiles :: FilePath -> [String] -> IO [FilePath]
packageGlobToCabalFiles here = \case
  [] -> pure [here]
  ("" : rest) -> packageGlobToCabalFiles here rest
  ("." : rest) -> packageGlobToCabalFiles here rest
  (component : rest)
    | '*' `elem` component -> do
        entries <- quietly [] (listDirectory here)
        concat
          <$> traverse
            (\e -> packageGlobToCabalFiles (here </> e) rest)
            (sort (filter (globMatching component) entries))
    | otherwise -> packageGlobToCabalFiles (here </> component) rest

-- | Does a name match a pattern with @*@ in it?
globMatching :: String -> String -> Bool
globMatching pattern name = case break (== '*') pattern of
  (before, []) -> before == name
  (before, _ : after) ->
    before `isPrefixOf` name
      && after `isSuffixOf` drop (length before) name

-- | The @.cabal@ files sitting directly in a directory.
cabalFilesIn :: FilePath -> IO [FilePath]
cabalFilesIn directory = quietly [] $ do
  entries <- listDirectory directory
  let named = sort (filter (".cabal" `isSuffixOf`) entries)
  filterM doesFileExist (fmap (directory </>) named)

-- | Every component one @.cabal@ file declares.
componentsInCabalFile :: FilePath -> IO (Either TargetProblem [Component])
componentsInCabalFile cabalFile =
  attempted (BS.readFile cabalFile) >>= \case
    Left why -> pure (Left (Unparseable cabalFile [why]))
    Right bytes ->
      case snd (runParseResult (parseGenericPackageDescription bytes)) of
        Left (_, complaints) ->
          pure (Left (Unparseable cabalFile (fmap said (NE.toList complaints))))
          where
            said = T.pack . showPError cabalFile
        Right described ->
          pure (Right (declaredComponents (takeDirectory cabalFile) described))

-- | The components of a parsed @.cabal@ file, in the order declared.
declaredComponents :: FilePath -> GenericPackageDescription -> [Component]
declaredComponents root described =
  concat
    [ foldMap
        (pure . made Lib package . libBuildInfo . condTreeData)
        (condLibrary described),
      [ made Lib (nameOf n) (libBuildInfo (condTreeData t))
      | (n, t) <- condSubLibraries described
      ],
      [ made Exe (nameOf n) (buildInfo (condTreeData t))
      | (n, t) <- condExecutables described
      ],
      [ made Test (nameOf n) (testBuildInfo (condTreeData t))
      | (n, t) <- condTestSuites described
      ],
      [ made Bench (nameOf n) (benchmarkBuildInfo (condTreeData t))
      | (n, t) <- condBenchmarks described
      ]
    ]
  where
    package =
      T.pack (unPackageName (pkgName (package' described)))
    package' = Distribution.PackageDescription.package . packageDescription
    nameOf = T.pack . unUnqualComponentName
    made kind name bi =
      Component
        { componentPackage = package,
          componentKind = kind,
          componentName = name,
          componentRoot = root,
          componentDirs = case fmap getSymbolicPath (hsSourceDirs bi) of
            [] -> ["."]
            ds -> ds
        }
