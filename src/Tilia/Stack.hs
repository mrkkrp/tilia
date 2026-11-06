{-# LANGUAGE DataKinds #-}

-- | The compiler Stack builds a project with.
module Tilia.Stack
  ( useStackCompiler,
  )
where

import Control.Monad (mfilter, (>=>))
import Data.Choice (Choice)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Text qualified as T
import System.Directory (doesDirectoryExist)
import System.Environment (lookupEnv, setEnv)
import System.FilePath (searchPathSeparator, takeDirectory)

import Tilia.Cabal.Project
  ( Marker (..),
    ProjectRoot (..),
    stackConfigAbove,
    stackOutputIn,
  )
import Tilia.Fixity.Cache
  ( PlanToken (..),
    cachedStackCompiler,
    openCache,
    storeStackCompiler,
  )
import Tilia.Utils (quietly, sha256Of)

-- | Put the compiler Stack builds a project with first on this process's
-- path, where the project is a package under a @stack.yaml@ rather than a
-- @cabal.project@, so that @cabal@, @ghc@ and @ghc-pkg@ are that compiler's
-- in everything Tilia runs.
useStackCompiler ::
  -- | Whether to read from and write to the cache.
  Choice "useCache" ->
  -- | The project.
  ProjectRoot ->
  IO ()
useStackCompiler caching root = case prMarker root of
  ProjectFile -> pure ()
  PackageFile _ ->
    stackConfigAbove (prPath root)
      >>= traverse_ (compilerDirFor caching >=> traverse_ prependToPath)

-- | The directory holding the compiler Stack builds the project of a
-- @stack.yaml@ with, asked of Stack again only once the @stack.yaml@ or its
-- lock file changes.
compilerDirFor ::
  -- | Whether to read from and write to the cache.
  Choice "useCache" ->
  -- | The @stack.yaml@.
  FilePath ->
  IO (Maybe FilePath)
compilerDirFor caching config = do
  cache <- openCache caching (PlanToken (T.take 16 (sha256Of (T.pack config))))
  remembered <- cachedStackCompiler cache
  present <- maybe (pure False) (quietly False . doesDirectoryExist) remembered
  if present
    then pure remembered
    else do
      said <- stackOutputIn (takeDirectory config) ["path", "--compiler-bin"]
      let dir = mfilter (not . null) (T.unpack . T.strip <$> said)
      traverse_ (storeStackCompiler cache [config, config <> ".lock"]) dir
      pure dir

-- | Put a directory first on this process's path.
prependToPath :: FilePath -> IO ()
prependToPath dir = do
  path <- lookupEnv "PATH"
  setEnv "PATH" . intercalate [searchPathSeparator] $
    dir : [p | Just p <- [path], not (null p)]
