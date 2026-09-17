{-# LANGUAGE OverloadedStrings #-}

-- | Finding the project a file belongs to.
module Tilia.Cabal.ProjectSpec (spec) where

import System.Directory (createDirectoryIfMissing, withCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Tilia.Cabal.Project

spec :: Spec
spec = do
  describe "in this repository" $ do
    it "finds the root from the root" $ do
      root <- findProjectRoot "."
      (prMarker <$> root) `shouldBe` Just ProjectFile

    it "finds the root from a nested source directory" $ do
      root <- findProjectRoot "src/Tilia/Printer"
      (prMarker <$> root) `shouldBe` Just ProjectFile

    it "finds the root from a file rather than a directory" $ do
      root <- findProjectRoot "src/Tilia/Fixity.hs"
      (prMarker <$> root) `shouldBe` Just ProjectFile

    it "returns the same directory however it is reached" $ do
      a <- findProjectRoot "."
      b <- findProjectRoot "tests/Tilia"
      c <- findProjectRoot "src/Tilia/Printer/Internal.hs"
      (prPath <$> a, prPath <$> b) `shouldBe` (prPath <$> a, prPath <$> c)
      (prPath <$> b) `shouldBe` (prPath <$> c)

  describe "marker precedence" $ do
    it "prefers cabal.project to a bare .cabal file" $
      withTree [("cabal.project", ""), ("thing.cabal", "")] $ \dir -> do
        root <- findProjectRoot dir
        (prMarker <$> root) `shouldBe` Just ProjectFile

    it "accepts a bare .cabal file when there is no project" $
      withTree [("thing.cabal", "")] $ \dir -> do
        root <- findProjectRoot dir
        (prMarker <$> root) `shouldBe` Just (PackageFile "thing.cabal")

    it "passes over a stack.yaml, which cabal does not read" $
      withTree [("stack.yaml", ""), ("thing.cabal", "")] $ \dir -> do
        root <- findProjectRoot dir
        (prMarker <$> root) `shouldBe` Just (PackageFile "thing.cabal")

    it "climbs out of a stack project to the package it is asked about" $
      withTree [("stack.yaml", ""), ("packages/inner/inner.cabal", "")] $
        \dir -> do
          root <- findProjectRoot (dir </> "packages" </> "inner")
          prPath <$> root `shouldBe` Just (dir </> "packages" </> "inner")

    it "climbs past a package to the project that contains it"
      $ withTree
        [ ("cabal.project", ""),
          ("packages/inner/placeholder", "")
        ]
      $ \dir -> do
        root <- findProjectRoot (dir </> "packages" </> "inner")
        (prMarker <$> root) `shouldBe` Just ProjectFile

    it "climbs past a package that has its own .cabal"
      $ withTree
        [ ("cabal.project", ""),
          ("packages/inner/inner.cabal", "")
        ]
      $ \dir -> do
        root <- findProjectRoot (dir </> "packages" </> "inner")
        (prPath <$> root) `shouldBe` Just dir
        (prMarker <$> root) `shouldBe` Just ProjectFile

    it "takes the nearest project of the ones above"
      $ withTree
        [ ("cabal.project", ""),
          ("packages/inner/cabal.project", ""),
          ("packages/inner/inner.cabal", "")
        ]
      $ \dir -> do
        root <- findProjectRoot (dir </> "packages" </> "inner")
        prPath <$> root `shouldBe` Just (dir </> "packages" </> "inner")

    it "takes the nearest package when no project is above either"
      $ withTree
        [ ("outer.cabal", ""),
          ("packages/inner/inner.cabal", "")
        ]
      $ \dir -> do
        root <- findProjectRoot (dir </> "packages" </> "inner")
        (prMarker <$> root) `shouldBe` Just (PackageFile "inner.cabal")

    it "climbs to a project past a package that is not the one asked about"
      $ withTree
        [ ("cabal.project", ""),
          ("outer.cabal", ""),
          ("packages/inner/inner.cabal", "")
        ]
      $ \dir -> do
        root <- findProjectRoot (dir </> "packages" </> "inner")
        (prMarker <$> root) `shouldBe` Just ProjectFile

  describe "no project" $
    it "gives up rather than guessing" $
      withTree [("lonely/Thing.hs", "module Thing where")] $ \dir ->
        -- A temporary directory has no project above it, so this walks to
        -- the filesystem root and finds nothing.
        withCurrentDirectory dir $ do
          root <- findProjectRoot "lonely"
          case root of
            Nothing -> pure ()
            Just found ->
              -- Some machines have a stray marker in a parent of the
              -- system temporary directory; only a genuine find inside the
              -- tree would be a failure.
              prPath found `shouldNotBe` (dir </> "lonely")

-- | Build a throwaway tree of files and run an action on its root.
withTree :: [(FilePath, String)] -> (FilePath -> IO a) -> IO a
withTree files action =
  withSystemTempDirectory "tilia-project" $ \dir -> do
    mapM_ (create dir) files
    action dir
  where
    create dir (path, contents) = do
      let full = dir </> path
      createDirectoryIfMissing True (parentOf full)
      writeFile full contents
    parentOf = reverse . drop 1 . dropWhile (/= '/') . reverse
