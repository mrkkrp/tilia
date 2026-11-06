{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The compiler Stack builds a project with.
module Tilia.StackSpec (spec) where

import Data.Choice (pattern Do)
import Data.Time (addUTCTime, getCurrentTime)
import System.Directory (setModificationTime)
import System.Environment (lookupEnv)
import System.FilePath (searchPathSeparator, (</>))
import Test.Hspec

import Tilia.Cabal.Project (Marker (..), ProjectRoot (..))
import Tilia.Stack (useStackCompiler)
import Tilia.TestUtils (withEnvironment, withFiles, withStandIn)

spec :: Spec
spec = describe "useStackCompiler" $ do
  it "puts the directory Stack names first on the path" $
    withStackPackage $ \dir -> do
      (ran, path) <- withStandIn "stack" (naming dir) $ do
        useStackCompiler (Do #useCache) (package dir)
        lookupEnv "PATH"
      length ran `shouldBe` 1
      takeWhile (/= searchPathSeparator) <$> path
        `shouldBe` Just (dir </> "bin")

  it "asks Stack again only once its configuration changes" $
    withStackPackage $ \dir -> do
      (ran, ()) <- withStandIn "stack" (naming dir) $ do
        useStackCompiler (Do #useCache) (package dir)
        useStackCompiler (Do #useCache) (package dir)
        later <- addUTCTime 60 <$> getCurrentTime
        setModificationTime (dir </> "stack.yaml") later
        useStackCompiler (Do #useCache) (package dir)
      length ran `shouldBe` 2

  it "leaves the path alone for a cabal.project" $
    withStackPackage $ \dir -> do
      (ran, (was, is)) <- withStandIn "stack" "exit 1" $ do
        was <- lookupEnv "PATH"
        useStackCompiler (Do #useCache) (ProjectRoot dir ProjectFile)
        is <- lookupEnv "PATH"
        pure (was, is)
      ran `shouldBe` []
      is `shouldBe` was
  where
    package dir = ProjectRoot dir (PackageFile "thing.cabal")
    naming dir = "echo '" <> dir </> "bin" <> "'"

-- | A package under a @stack.yaml@, with a directory @bin@ for Stack to
-- name and a cache of its own.
withStackPackage :: (FilePath -> IO a) -> IO a
withStackPackage act =
  withFiles
    [("stack.yaml", ""), ("thing.cabal", ""), ("bin/placeholder", "")]
    $ \dir -> withEnvironment [("XDG_CACHE_HOME", dir </> "cache")] (act dir)
