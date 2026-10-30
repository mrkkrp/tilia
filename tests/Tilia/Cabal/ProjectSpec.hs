{-# LANGUAGE OverloadedStrings #-}

-- | Finding the project a file belongs to.
module Tilia.Cabal.ProjectSpec (spec) where

import System.FilePath ((</>))
import Test.Hspec
import Tilia.Cabal.Project
import Tilia.TestUtils (withFiles)

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
      withFiles [("cabal.project", ""), ("thing.cabal", "")] $ \dir -> do
        root <- findProjectRoot dir
        (prMarker <$> root) `shouldBe` Just ProjectFile

    it "accepts a bare .cabal file when there is no project" $
      withFiles [("thing.cabal", "")] $ \dir -> do
        root <- findProjectRoot dir
        (prMarker <$> root) `shouldBe` Just (PackageFile "thing.cabal")

    it "passes over a stack.yaml, which cabal does not read" $
      withFiles [("stack.yaml", ""), ("thing.cabal", "")] $ \dir -> do
        root <- findProjectRoot dir
        (prMarker <$> root) `shouldBe` Just (PackageFile "thing.cabal")

    it "climbs out of a stack project to the package it is asked about" $
      withFiles [("stack.yaml", ""), ("packages/inner/inner.cabal", "")] $
        \dir -> do
          root <- findProjectRoot (dir </> "packages" </> "inner")
          prPath <$> root `shouldBe` Just (dir </> "packages" </> "inner")

    it "climbs past a package to the project that contains it"
      $ withFiles
        [ ("cabal.project", ""),
          ("packages/inner/placeholder", "")
        ]
      $ \dir -> do
        root <- findProjectRoot (dir </> "packages" </> "inner")
        (prMarker <$> root) `shouldBe` Just ProjectFile

    it "climbs past a package that has its own .cabal"
      $ withFiles
        [ ("cabal.project", ""),
          ("packages/inner/inner.cabal", "")
        ]
      $ \dir -> do
        root <- findProjectRoot (dir </> "packages" </> "inner")
        (prPath <$> root) `shouldBe` Just dir
        (prMarker <$> root) `shouldBe` Just ProjectFile

    it "takes the nearest project of the ones above"
      $ withFiles
        [ ("cabal.project", ""),
          ("packages/inner/cabal.project", ""),
          ("packages/inner/inner.cabal", "")
        ]
      $ \dir -> do
        root <- findProjectRoot (dir </> "packages" </> "inner")
        prPath <$> root `shouldBe` Just (dir </> "packages" </> "inner")

    it "takes the nearest package when no project is above either"
      $ withFiles
        [ ("outer.cabal", ""),
          ("packages/inner/inner.cabal", "")
        ]
      $ \dir -> do
        root <- findProjectRoot (dir </> "packages" </> "inner")
        (prMarker <$> root) `shouldBe` Just (PackageFile "inner.cabal")

    it "climbs to a project past a package that is not the one asked about"
      $ withFiles
        [ ("cabal.project", ""),
          ("outer.cabal", ""),
          ("packages/inner/inner.cabal", "")
        ]
      $ \dir -> do
        root <- findProjectRoot (dir </> "packages" </> "inner")
        (prMarker <$> root) `shouldBe` Just ProjectFile

  describe "no project" $
    it "gives up rather than guessing" $
      withFiles [("lonely/Thing.hs", "module Thing where")] $ \dir -> do
        root <- findProjectRoot (dir </> "lonely")
        case root of
          Nothing -> pure ()
          Just found ->
            prPath found `shouldNotBe` (dir </> "lonely")
