{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Working with cabal targets.
module Tilia.Cabal.TargetSpec (spec) where

import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Tilia.Cabal.Project (Marker (..), ProjectRoot (..), findProjectRoot)
import Tilia.Cabal.Target

spec :: Spec
spec = do
  describe "reading a target as it was written" $ do
    it "takes all as everything" $
      parseTarget "all" `shouldBe` Right Everything

    it "takes a bare word as a name that may be either" $
      parseTarget "tilia" `shouldBe` Right (Called "tilia")

    it "takes each kind of component" $ do
      parseTarget "lib:tilia" `shouldBe` Right (Qualified Nothing Lib "tilia")
      parseTarget "exe:tilia" `shouldBe` Right (Qualified Nothing Exe "tilia")
      parseTarget "test:tests" `shouldBe` Right (Qualified Nothing Test "tests")
      parseTarget "bench:speed" `shouldBe` Right (Qualified Nothing Bench "speed")

    it "takes benchmark as a spelling of bench" $
      parseTarget "benchmark:speed" `shouldBe` Right (Qualified Nothing Bench "speed")

    it "takes a package in front of the kind" $
      parseTarget "tilia:lib:tilia" `shouldBe` Right (Qualified (Just "tilia") Lib "tilia")

    it "ignores space around it" $
      parseTarget "  lib:tilia  " `shouldBe` Right (Qualified Nothing Lib "tilia")

    it "refuses an empty target" $
      parseTarget "" `shouldSatisfy` failed

    it "refuses a kind it does not know" $
      parseTarget "flib:thing" `shouldSatisfy` failed

    it "refuses more colons than it can account for" $
      parseTarget "a:lib:b:c" `shouldSatisfy` failed

    it "says what it would have accepted" $
      case parseTarget "flib:thing" of
        Left why -> why `shouldSatisfy` T.isInfixOf "lib:"
        Right _ -> expectationFailure "should not have parsed"

  describe "against this very project" $ do
    root <- runIO (findProjectRoot ".")
    case root of
      Nothing -> it "needs a project" $ pendingWith "no project above the working directory"
      Just here -> do
        it "is rooted at the cabal.project, not the .cabal file" $
          prMarker here `shouldBe` ProjectFile

        it "finds the three components this package declares" $
          componentsOfTarget here Everything >>= \case
            Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
            Right cs ->
              sort (fmap (\c -> (componentKind c, componentName c)) cs)
                `shouldBe` sort [(Lib, "tilia"), (Exe, "tilia"), (Test, "tests")]

        it "narrows to one component when asked for one" $
          componentsOfTarget here (Qualified Nothing Lib "tilia") >>= \case
            Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
            Right cs -> do
              files <- filesOfComponents here cs
              files `shouldSatisfy` any ("src/Tilia/Cabal/Target.hs" `isSuffixOf`)

        it "takes the package name as all of its components" $
          componentsOfTarget here (Called "tilia") >>= \case
            Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
            Right cs -> length cs `shouldBe` 3

        it "refuses a target the project does not hold, and says what it does" $
          componentsOfTarget here (Called "nothing-like-this") >>= \case
            Right cs -> expectationFailure ("matched " <> show (length cs) <> " components")
            Left problem -> do
              let said = describeTargetProblem problem
              said `shouldSatisfy` T.isInfixOf "tilia:lib:tilia"
              said `shouldSatisfy` T.isInfixOf "tilia:test:tests"
              said `shouldSatisfy` T.isInfixOf "\n  all"

        it "finds this module among the test component's files" $
          componentsOfTarget here (Qualified Nothing Test "tests") >>= \case
            Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
            Right cs -> do
              files <- filesOfComponents here cs
              fmap takeFileName files `shouldSatisfy` elem "TargetSpec.hs"

        it "finds only Haskell in it" $
          componentsOfTarget here Everything >>= \case
            Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
            Right cs -> do
              files <- filesOfComponents here cs
              filter (not . haskell) files `shouldBe` []

        it "does not wander into the build directory" $
          componentsOfTarget here Everything >>= \case
            Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
            Right cs -> do
              files <- filesOfComponents here cs
              filter (T.isInfixOf "dist-newstyle" . T.pack) files `shouldBe` []

  describe "against a project made up for the purpose" $ do
    it "reads the packages a cabal.project names"
      $ withProject
        [ ("cabal.project", "packages: one two\n"),
          ("one/one.cabal", package "one" "src"),
          ("one/src/A.hs", "module A where\n"),
          ("two/two.cabal", package "two" "lib"),
          ("two/lib/B.hs", "module B where\n")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> sort (fmap componentPackage cs) `shouldBe` ["one", "two"]

    it "expands a glob in the packages field"
      $ withProject
        [ ("cabal.project", "packages: pkgs/*/*.cabal\n"),
          ("pkgs/one/one.cabal", package "one" "src"),
          ("pkgs/two/two.cabal", package "two" "src")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> sort (fmap componentPackage cs) `shouldBe` ["one", "two"]

    it "passes over a package a comment has taken out"
      $ withProject
        [ ("cabal.project", "packages:\n  one\n  -- two\n"),
          ("one/one.cabal", package "one" "src"),
          ("two/two.cabal", package "two" "src")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> fmap componentPackage cs `shouldBe` ["one"]

    it "reads a packages field continued onto later lines"
      $ withProject
        [ ("cabal.project", "packages:\n  one\n  two\n"),
          ("one/one.cabal", package "one" "src"),
          ("two/two.cabal", package "two" "src")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> sort (fmap componentPackage cs) `shouldBe` ["one", "two"]

    it "reads one continued with tabs, as cabal itself does"
      $ withProject
        [ ("cabal.project", "packages:\n\tone\n\ttwo\n"),
          ("one/one.cabal", package "one" "src"),
          ("two/two.cabal", package "two" "src")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> sort (fmap componentPackage cs) `shouldBe` ["one", "two"]

    it "finds one a conditional has put inside a section"
      $ withProject
        [ ("cabal.project", "if impl(ghc >= 9.4)\n  packages: one\n"),
          ("one/one.cabal", package "one" "src")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> fmap componentPackage cs `shouldBe` ["one"]

    it "looks in every source directory a component names"
      $ withProject
        [ ("only.cabal", packageWith "only" ["src", "gen"] ["A", "B"]),
          ("src/A.hs", "module A where\n"),
          ("gen/B.hs", "module B where\n")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> do
            files <- filesOfComponents root cs
            sort (fmap takeFileName files) `shouldBe` ["A.hs", "B.hs"]

    it "spells a path through a dot source directory without the dot"
      $ withProject
        [ ("only.cabal", packageWith "only" ["."] ["A", "Nested.B"]),
          ("A.hs", "module A where\n"),
          ("Nested/B.hs", "module Nested.B where\n")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> do
            files <- filesOfComponents root cs
            filter (T.isInfixOf "/./" . T.pack) files `shouldBe` []

    it "spells a path through a dot-slash source directory without the dot"
      $ withProject
        [ ("only.cabal", packageWith "only" ["./"] ["A"]),
          ("A.hs", "module A where\n")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> do
            files <- filesOfComponents root cs
            files `shouldBe` [prPath root </> "A.hs"]

    it "names a file once even when two components reach it"
      $ withProject
        [ ("both.cabal", twoComponents),
          ("app/One.hs", "module Main where\n"),
          ("app/Two.hs", "module Main where\n"),
          ("app/Shared.hs", "module Shared where\n")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> do
            files <- filesOfComponents root cs
            length cs `shouldBe` 2
            fmap takeFileName files `shouldBe` ["One.hs", "Shared.hs", "Two.hs"]

    it "leaves out files no component declares"
      $ withProject
        [ ("only.cabal", packageWith "only" ["."] ["A"]),
          ("A.hs", "module A where\n"),
          ("Stray.hs", "module Stray where\n"),
          ("data/Example.hs", "main = pure ()\n")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> do
            files <- filesOfComponents root cs
            fmap takeFileName files `shouldBe` ["A.hs"]

    it "takes the boot file and signature of a declared module"
      $ withProject
        [ ("only.cabal", packageWith "only" ["src"] ["A", "B"]),
          ("src/A.hs", "module A where\n"),
          ("src/A.hs-boot", "module A where\n"),
          ("src/B.hsig", "signature B where\n")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> do
            files <- filesOfComponents root cs
            fmap takeFileName files `shouldBe` ["A.hs", "A.hs-boot", "B.hsig"]

    it "takes what every conditional branch declares"
      $ withProject
        [ ("only.cabal", conditional),
          ("src/A.hs", "module A where\n"),
          ("unix/B.hs", "module B where\n"),
          ("windows/C.hs", "module C where\n")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> do
            files <- filesOfComponents root cs
            fmap takeFileName files `shouldBe` ["A.hs", "B.hs", "C.hs"]

    it "looks for inherited modules in the directories a branch adds"
      $ withProject
        [ ("only.cabal", platformSpecific),
          ("src/A.hs", "module A where\n"),
          ("unix/B.hs", "module B where\n"),
          ("windows/B.hs", "module B where\n")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> do
            files <- filesOfComponents root cs
            files
              `shouldBe` fmap (prPath root </>) ["src/A.hs", "unix/B.hs", "windows/B.hs"]

    it "passes over a declared module with no source, such as a generated one"
      $ withProject
        [ ("only.cabal", packageWith "only" ["src"] ["A", "Paths_only"]),
          ("src/A.hs", "module A where\n")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> do
            files <- filesOfComponents root cs
            fmap takeFileName files `shouldBe` ["A.hs"]

    it "excludes literal files and directory trees from .tiliaignore"
      $ withProject
        [ ( "only.cabal",
            packageWith
              "only"
              ["src"]
              ["Runner", "Generated", "Fixtures.Input", "Fixtures.Nested.Other", "FixturesOther.Keep"]
          ),
          (".tiliaignore", "  # Generated sources and runtime fixtures\r\n\r\n ./src/Fixtures/ \r\nsrc/Generated.hs\r\n"),
          ("src/Runner.hs", "module Runner where\n"),
          ("src/Generated.hs", "module Generated where\n"),
          ("src/Fixtures/Input.hs", "module Fixtures.Input where\n"),
          ("src/Fixtures/Nested/Other.hs", "module Fixtures.Nested.Other where\n"),
          ("src/FixturesOther/Keep.hs", "module FixturesOther.Keep where\n")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> do
            files <- filesOfComponents root cs
            sort (map takeFileName files) `shouldBe` ["Keep.hs", "Runner.hs"]

    it "uses the project ignore file for packages and explicit fixture source directories"
      $ withProject
        [ ("cabal.project", "packages: one two\n"),
          (".tiliaignore", "one/fixtures\n"),
          ("one/one.cabal", packageWith "one" ["src", "fixtures"] ["A", "Input"]),
          ("one/src/A.hs", "module A where\n"),
          ("one/fixtures/Input.hs", "module Input where\n"),
          ("two/two.cabal", packageWith "two" ["fixtures"] ["B"]),
          ("two/fixtures/B.hs", "module B where\n")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> do
            files <- filesOfComponents root cs
            sort (map takeFileName files) `shouldBe` ["A.hs", "B.hs"]

    it "can exclude every file of an explicitly selected component"
      $ withProject
        [ ("only.cabal", packageWith "only" ["fixtures"] ["Input"]),
          (".tiliaignore", "fixtures/\n"),
          ("fixtures/Input.hs", "module Input where\n")
        ]
      $ \root ->
        componentsOfTarget root (Called "only") >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> filesOfComponents root cs `shouldReturn` []

    it "takes the setup script under all and the package name, not under a component"
      $ withProject
        [ ("only.cabal", packageWith "only" ["src"] ["A"]),
          ("Setup.hs", "import Distribution.Simple\nmain = defaultMain\n"),
          ("src/A.hs", "module A where\n")
        ]
      $ \root -> do
        let filesFor target =
              componentsOfTarget root target >>= \case
                Left problem -> fail (T.unpack (describeTargetProblem problem))
                Right cs -> fmap takeFileName <$> filesOfComponents root cs
        filesFor Everything `shouldReturn` ["Setup.hs", "A.hs"]
        filesFor (Called "only") `shouldReturn` ["Setup.hs", "A.hs"]
        filesFor (Qualified Nothing Lib "only") `shouldReturn` ["A.hs"]

    it "does not offer the setup script as a target"
      $ withProject
        [ ("only.cabal", packageWith "only" ["src"] ["A"]),
          ("Setup.hs", "import Distribution.Simple\nmain = defaultMain\n")
        ]
      $ \root ->
        componentsOfTarget root (Called "setup") >>= \case
          Right cs -> expectationFailure ("matched " <> show (length cs) <> " components")
          Left problem -> describeTargetProblem problem `shouldNotSatisfy` T.isInfixOf ":setup"

    it "says so when a cabal.project names nothing that exists" $
      withProject [("cabal.project", "packages: nowhere\n")] $ \root ->
        componentsOfTarget root Everything >>= \case
          Right cs -> expectationFailure ("found " <> show (length cs) <> " components")
          Left problem -> describeTargetProblem problem `shouldSatisfy` T.isInfixOf "no packages"

    it "includes optional packages that exist, ignoring absent ones"
      $ withProject
        [ ("cabal.project", "packages: main\noptional-packages: optional absent\n"),
          ("main/main.cabal", package "main" "src"),
          ("optional/optional.cabal", package "optional" "src")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> sort (map componentPackage cs) `shouldBe` ["main", "optional"]

    it "says so when a .cabal file will not parse"
      $ withProject
        [ ("cabal.project", "packages: .\n"),
          ("broken.cabal", "this is not a cabal file at all\n")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Right cs -> expectationFailure ("found " <> show (length cs) <> " components")
          Left problem -> describeTargetProblem problem `shouldSatisfy` T.isInfixOf "does not parse"

----------------------------------------------------------------------------
-- Helpers

failed :: Either Text Target -> Bool
failed = \case
  Left _ -> True
  Right _ -> False

haskell :: FilePath -> Bool
haskell path = any (`isSuffixOf` path) [".hs", ".hs-boot", ".hsig"]

-- | A @.cabal@ file for a package with one library that declares no
-- modules.
package :: Text -> Text -> Text
package name dir = packageWith name [dir] []

-- | A @.cabal@ file for a package with one library.
packageWith ::
  -- | The name of the package.
  Text ->
  -- | Its source directories.
  [Text] ->
  -- | The modules it exposes.
  [Text] ->
  Text
packageWith name dirs modules =
  T.unlines
    [ "cabal-version: 2.4",
      "name: " <> name,
      "version: 0.1.0.0",
      "",
      "library",
      "  hs-source-dirs: " <> T.intercalate ", " dirs,
      "  exposed-modules: " <> T.intercalate ", " modules,
      "  default-language: Haskell2010"
    ]

-- | Lay out a project in a temporary directory and hand over its root.
withProject :: [(FilePath, Text)] -> (ProjectRoot -> IO a) -> IO a
withProject files act =
  withSystemTempDirectory "tilia-target" $ \directory -> do
    mapM_ (place directory) files
    act (ProjectRoot directory (marker files))
  where
    place directory (path, contents) = do
      createDirectoryIfMissing True (directory </> parent path)
      T.writeFile (directory </> path) contents
    parent = reverse . drop 1 . dropWhile (/= '/') . reverse
    marker fs
      | any ((== "cabal.project") . fst) fs = ProjectFile
      | (named : _) <- [p | (p, _) <- fs, ".cabal" `isSuffixOf` p] = PackageFile named
      | otherwise = ProjectFile

-- | A package with two executables that share a module.
twoComponents :: Text
twoComponents =
  T.unlines
    [ "cabal-version: 2.4",
      "name: both",
      "version: 0.1.0.0",
      "",
      "executable one",
      "  main-is: One.hs",
      "  hs-source-dirs: app",
      "  other-modules: Shared",
      "  default-language: Haskell2010",
      "",
      "executable two",
      "  main-is: Two.hs",
      "  hs-source-dirs: app",
      "  other-modules: Shared",
      "  default-language: Haskell2010"
    ]

-- | A package whose library declares its modules once and finds one of
-- them in a directory that depends on the platform.
platformSpecific :: Text
platformSpecific =
  T.unlines
    [ "cabal-version: 2.4",
      "name: only",
      "version: 0.1.0.0",
      "",
      "library",
      "  hs-source-dirs: src",
      "  exposed-modules: A, B",
      "  default-language: Haskell2010",
      "  if os(windows)",
      "    hs-source-dirs: windows",
      "  else",
      "    hs-source-dirs: unix"
    ]

-- | A package whose library declares a module in each branch of a
-- conditional, each from a source directory of its own.
conditional :: Text
conditional =
  T.unlines
    [ "cabal-version: 2.4",
      "name: only",
      "version: 0.1.0.0",
      "",
      "library",
      "  hs-source-dirs: src",
      "  exposed-modules: A",
      "  default-language: Haskell2010",
      "  if os(windows)",
      "    hs-source-dirs: windows",
      "    other-modules: C",
      "  else",
      "    hs-source-dirs: unix",
      "    other-modules: B"
    ]
