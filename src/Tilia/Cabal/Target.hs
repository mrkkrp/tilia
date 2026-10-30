{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Handling Cabal targets in order to figure out what to format.
module Tilia.Cabal.Target
  ( Target (..),
    Kind (..),
    parseTarget,
    Component (..),
    Wanted (..),
    TargetProblem,
    describeTargetProblem,
    componentsOfTarget,
    componentInPlan,
    filesOfComponents,
    packageFilesOf,
  )
where

import Control.Monad (filterM)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (toLower)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Distribution.Fields.Field (Field (..), FieldLine (..), Name (..))
import Distribution.Fields.Parser (readFields)
import Distribution.PackageDescription
  ( GenericPackageDescription (..),
    PackageDescription (..),
    unPackageName,
    unUnqualComponentName,
  )
import Distribution.Types.PackageId (PackageIdentifier (..))
import System.Directory (canonicalizePath, doesFileExist, listDirectory)
import System.FilePath
  ( dropTrailingPathSeparator,
    normalise,
    takeDirectory,
    (</>),
  )
import Tilia.Cabal.Package
  ( ComponentSection (..),
    PackageProblem,
    addedSources,
    benchmarkBranches,
    describePackageProblem,
    executableBranches,
    isDeclaredModule,
    joined,
    libraryBranches,
    readPackageDescription,
    setupScript,
    suiteBranches,
  )
import Tilia.Cabal.Plan (PlanComponent (..))
import Tilia.Cabal.Project (Marker (..), ProjectRoot (..), cabalFilesIn, markerFile)
import Tilia.Ignore (withoutIgnored)
import Tilia.Utils (fieldWords, quietly, tshow)

-- | Which components a run was asked for.
data Target
  = -- | @all@, or nothing given at all: every component of every package.
    Everything
  | -- | One bare word, which may name a package or a component.
    Called Text
  | -- | @kind:name@, or @package:kind:name@.
    Qualified (Maybe Text) Kind Text
  deriving (Eq, Show)

-- | The kinds of component a @.cabal@ file can declare, and the setup
-- script beside it.
data Kind = Lib | Exe | Test | Bench | Setup
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
        <> tshow written
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
  Called name
    | Setup <- componentKind c -> name == componentPackage c
    | otherwise -> name == componentName c || name == componentPackage c
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
    -- | Its name, which for the main library is the package's own.
    componentName :: Text,
    -- | The directory its @.cabal@ file sits in.
    componentRoot :: FilePath,
    -- | The directories its declared files may be in, relative to
    -- 'componentRoot', across all of its conditional branches.
    componentSources :: Map FilePath Wanted
  }
  deriving (Eq, Show)

-- | What may be in one directory.
data Wanted = Wanted
  { -- | Modules, by file name without an extension.
    wantedModules :: Set FilePath,
    -- | Entry points, by file name.
    wantedEntries :: Set FilePath
  }
  deriving (Eq, Show)

instance Semigroup Wanted where
  Wanted m e <> Wanted m' e' = Wanted (m <> m') (e <> e')

-- | Is a directory entry one of the files wanted there?
admits :: Wanted -> FilePath -> Bool
admits Wanted{..} = isDeclaredModule wantedModules wantedEntries

-- | Why a run could not work out what to format.
data TargetProblem
  = -- | A @cabal.project@ naming packages, none of which could be found.
    NoPackages FilePath
  | -- | A @.cabal@ file that could not be read or parsed, and why.
    UnreadablePackage PackageProblem
  | -- | A target naming no component the project holds.
    NoSuchTarget Text [Text]
  deriving (Eq, Show)

-- | Say what went wrong.
describeTargetProblem :: TargetProblem -> Text
describeTargetProblem = \case
  NoPackages file ->
    T.pack file <> " names no packages that exist"
  UnreadablePackage problem -> describePackageProblem problem
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
                [] -> Left (NoSuchTarget (spellTarget target) (spellComponent <$> filter targetable found))
                wanted -> Right wanted

-- | Can a component be asked for on its own?
targetable :: Component -> Bool
targetable c = componentKind c /= Setup

-- | How a component would have to be named to be asked for on its own.
spellComponent :: Component -> Text
spellComponent c =
  componentPackage c
    <> ":"
    <> spellKind (componentKind c)
    <> ":"
    <> componentName c

-- | How a build plan names this component, if it has an entry of its own.
--
-- A plan writes the main library as @lib@ and everything else, sub-libraries
-- among them, as its kind and name, which is not quite how a target is
-- written: see 'spellComponent'.
-- A setup script has an entry only when the build type is @Custom@, so a
-- plan is never expected to say anything about it.
componentInPlan :: Component -> Maybe PlanComponent
componentInPlan c = case componentKind c of
  Setup -> Nothing
  kind ->
    Just
      PlanComponent
        { pcPackage = componentPackage c,
          pcName = case kind of
            Lib | componentName c == componentPackage c -> "lib"
            _ -> spellKind kind <> ":" <> componentName c
        }

-- | Render 'Kind' the way it would be accepted on the command line.
spellKind :: Kind -> Text
spellKind = \case
  Lib -> "lib"
  Exe -> "exe"
  Test -> "test"
  Bench -> "bench"
  Setup -> "setup"

-- | A target, written the way it would have been given.
spellTarget :: Target -> Text
spellTarget = \case
  Everything -> "all"
  Called name -> name
  Qualified package kind name ->
    T.intercalate ":" (foldMap pure package <> [spellKind kind, name])

-- | Every Haskell file a set of components holds, each named once, less
-- the ones the project's @.tiliaignore@ files exclude.
filesOfComponents :: ProjectRoot -> [Component] -> IO [FilePath]
filesOfComponents root components = do
  directories <-
    Map.fromListWith (<>)
      <$> sequence
        [ (,wanted) <$> quietly d (canonicalizePath d)
        | c <- components,
          (written, wanted) <- Map.toList (componentSources c),
          let d = joined (componentRoot c) written
        ]
  candidates <- concat <$> traverse present (Map.toList directories)
  sort <$> withoutIgnored (prPath root) candidates
  where
    present (directory, wanted) = do
      entries <- quietly [] (listDirectory directory)
      filterM
        (quietly False . doesFileExist)
        [joined directory entry | entry <- entries, admits wanted entry]

-- | A directory spelled so that a path can be appended to it without
-- further normalisation.
asDirectory :: FilePath -> FilePath
asDirectory = dropTrailingPathSeparator . normalise

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
    entries = fieldWords

-- | Turn one entry of a @packages@ field into the @.cabal@ files it names.
packageToCabalFile :: FilePath -> Text -> IO [FilePath]
packageToCabalFile root entry = do
  paths <-
    packageGlobToCabalFiles
      (if "/" `T.isPrefixOf` written then "/" else root)
      (fmap T.unpack (T.split (== '/') written))
  concat <$> traverse asPackage paths
  where
    written = T.strip entry
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

-- | Every component one @.cabal@ file declares.
componentsInCabalFile :: FilePath -> IO (Either TargetProblem [Component])
componentsInCabalFile cabalFile =
  readPackageDescription cabalFile >>= \case
    Left problem -> pure (Left (UnreadablePackage problem))
    Right described -> do
      let root = takeDirectory cabalFile
          declared = declaredComponents root described
      hasSetup <- quietly False (doesFileExist (root </> setupScript))
      pure . Right $
        declared
          <> [ Component
                 { componentPackage = packageOf described,
                   componentKind = Setup,
                   componentName = "setup",
                   componentRoot = root,
                   componentSources =
                     Map.singleton "." (Wanted Set.empty (Set.singleton setupScript))
                 }
             | hasSetup
             ]

-- | The name of a package.
packageOf :: GenericPackageDescription -> Text
packageOf =
  T.pack . unPackageName . pkgName . Distribution.PackageDescription.package . packageDescription

-- | The directories a branch's declarations may be found in under one
-- source directory.
sourcesIn :: FilePath -> ComponentSection -> Map FilePath Wanted
sourcesIn d ComponentSection{..} =
  Map.fromListWith
    (<>)
    ( [(joined d sub, Wanted (Set.singleton name) Set.empty) | (sub, name) <- sectionModules]
        <> [(joined d sub, Wanted Set.empty (Set.singleton name)) | (sub, name) <- sectionEntries]
    )

-- | The components of a parsed @.cabal@ file, in the order declared.
declaredComponents :: FilePath -> GenericPackageDescription -> [Component]
declaredComponents root described =
  concat
    [ foldMap
        (pure . made Lib package . libraryBranches)
        (condLibrary described),
      [made Lib (nameOf n) (libraryBranches t) | (n, t) <- condSubLibraries described],
      [made Exe (nameOf n) (executableBranches t) | (n, t) <- condExecutables described],
      [made Test (nameOf n) (suiteBranches t) | (n, t) <- condTestSuites described],
      [made Bench (nameOf n) (benchmarkBranches t) | (n, t) <- condBenchmarks described]
    ]
  where
    package = packageOf described
    nameOf = T.pack . unUnqualComponentName
    made kind name branches =
      Component
        { componentPackage = package,
          componentKind = kind,
          componentName = name,
          componentRoot = root,
          componentSources =
            Map.unionsWith
              (<>)
              [ sourcesIn (asDirectory d) declared
              | branch <- branches,
                (d, declared) <- addedSources branch
              ]
        }
