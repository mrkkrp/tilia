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
            Right cs -> fmap componentDirs cs `shouldBe` [["src"]]

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
              files <- filesOfComponents cs
              fmap takeFileName files `shouldSatisfy` elem "TargetSpec.hs"

        it "finds only Haskell in it" $
          componentsOfTarget here Everything >>= \case
            Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
            Right cs -> do
              files <- filesOfComponents cs
              filter (not . haskell) files `shouldBe` []

        it "does not wander into the build directory" $
          componentsOfTarget here Everything >>= \case
            Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
            Right cs -> do
              files <- filesOfComponents cs
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

    it "walks every source directory a component names"
      $ withProject
        [ ("only.cabal", packageWith "only" ["src", "gen"]),
          ("src/A.hs", "module A where\n"),
          ("gen/B.hs", "module B where\n")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> do
            files <- filesOfComponents cs
            sort (fmap takeFileName files) `shouldBe` ["A.hs", "B.hs"]

    it "spells a path through a dot source directory without the dot"
      $ withProject
        [ ("only.cabal", packageWith "only" ["."]),
          ("A.hs", "module A where\n"),
          ("nested/B.hs", "module B where\n")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> do
            files <- filesOfComponents cs
            filter (T.isInfixOf "/./" . T.pack) files `shouldBe` []

    it "names a file once even when two components reach it"
      $ withProject
        [ ("both.cabal", twoComponents),
          ("bench/Main.hs", "module Main where\n")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> do
            files <- filesOfComponents cs
            length cs `shouldBe` 2
            fmap takeFileName files `shouldBe` ["Main.hs"]

    it "leaves hidden directories alone"
      $ withProject
        [ ("only.cabal", package "only" "src"),
          ("src/A.hs", "module A where\n"),
          ("src/.hidden/B.hs", "module B where\n")
        ]
      $ \root ->
        componentsOfTarget root Everything >>= \case
          Left problem -> expectationFailure (T.unpack (describeTargetProblem problem))
          Right cs -> do
            files <- filesOfComponents cs
            fmap takeFileName files `shouldBe` ["A.hs"]

    it "says so when a cabal.project names nothing that exists" $
      withProject [("cabal.project", "packages: nowhere\n")] $ \root ->
        componentsOfTarget root Everything >>= \case
          Right cs -> expectationFailure ("found " <> show (length cs) <> " components")
          Left problem -> describeTargetProblem problem `shouldSatisfy` T.isInfixOf "no packages"

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

-- | A @.cabal@ file for a package with one library.
package :: Text -> Text -> Text
package name dir = packageWith name [dir]

packageWith :: Text -> [Text] -> Text
packageWith name dirs =
  T.unlines
    [ "cabal-version: 2.4",
      "name: " <> name,
      "version: 0.1.0.0",
      "",
      "library",
      "  hs-source-dirs: " <> T.intercalate ", " dirs,
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

-- | A package whose library sweeps the whole directory and whose benchmark
-- names a directory inside it, so that the two overlap.
twoComponents :: Text
twoComponents =
  T.unlines
    [ "cabal-version: 2.4",
      "name: both",
      "version: 0.1.0.0",
      "",
      "library",
      "  default-language: Haskell2010",
      "",
      "benchmark speed",
      "  type: exitcode-stdio-1.0",
      "  main-is: Main.hs",
      "  hs-source-dirs: bench",
      "  default-language: Haskell2010"
    ]
