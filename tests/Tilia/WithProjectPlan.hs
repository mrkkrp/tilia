{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This project, prepared the way the specs that read it need it.
module Tilia.WithProjectPlan
  ( withProjectPlan,
    compilerShipped,
  )
where

import Control.Monad (filterM, unless)
import Data.Maybe (isNothing, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec (Spec)
import Tilia.Cabal.Project (findProjectRoot)
import Tilia.Cabal.Target
  ( Target (..),
    componentInPlan,
    componentsOfTarget,
    describeTargetProblem,
  )
import Tilia.Fixity.PackageDb
  ( Installed (..),
    InstalledPackage (..),
    readInstalledPackages,
  )
import Tilia.Fixity.Plan
  ( BuildPlan,
    PlanPackage (..),
    loadPlan,
    plannedTarballs,
  )
import Tilia.Process (readProgramOutput)

-- | Build a spec around the build plan of this very project, with the
-- package sources that spec reads alongside it already fetched.
withProjectPlan :: (BuildPlan -> Spec) -> Spec
withProjectPlan use = either refuse use prepared
  where
    refuse why = error ("this project will not prepare: " <> T.unpack why)

-- | The answer 'withProjectPlan' hands out, worked out at most once.
prepared :: Either Text BuildPlan
prepared = unsafePerformIO prepare
{-# NOINLINE prepared #-}

-- | Find the project, have @cabal@ make it ready, and fetch what the specs
-- read that being ready does not cover.
prepare :: IO (Either Text BuildPlan)
prepare =
  findProjectRoot "." >>= \case
    Nothing -> pure (Left "no cabal.project or .cabal file above this directory")
    Just root ->
      componentsOfTarget root Everything >>= \case
        Left problem -> pure (Left (describeTargetProblem problem))
        Right components ->
          loadPlan (componentInPlan <$> components) "." >>= \case
            Left why -> pure (Left why)
            Right plan -> Right plan <$ fetchMissingSources plan

-- | Fetch the source of every planned package the specs read that is not
-- already here.
fetchMissingSources :: BuildPlan -> IO ()
fetchMissingSources plan = do
  installed <- readInstalledPackages
  let unread = shippedIn installed <> modulelessIn installed
  absent <-
    filterM (fmap not . doesFileExist . snd)
      . filter (not . (`Set.member` unread) . ppName . fst)
      =<< plannedTarballs plan
  unless (null absent) $ do
    putStrLn ("fetching the source of " <> show (length absent) <> " package(s)")
    refused <- filterM (fmap isNothing . fetch . fst) absent
    unless (null refused) $
      putStrLn ("no source on Hackage for " <> unwords (fmap (named . fst) refused))
  where
    fetch p = readProgramOutput "cabal" ["fetch", "--no-dependencies", named p]
    named p = T.unpack (ppName p <> "-" <> ppVersion p)

-- | The packages that came with the compiler rather than from a release of
-- their own, by the directory they are installed beside @ghc@ in.
compilerShipped :: IO (Set Text)
compilerShipped = shippedIn <$> readInstalledPackages

-- | The same, of packages already read.
shippedIn :: Installed -> Set Text
shippedIn installed = case compilerDir of
  Nothing -> Set.empty
  Just dir ->
    Set.fromList [ipName p | p <- installedPackages installed, beside p == Just dir]
  where
    beside p = takeDirectory <$> listToMaybe (ipImportDirs p)
    compilerDir =
      listToMaybe
        [dir | p <- installedPackages installed, ipName p == "ghc", Just dir <- [beside p]]

-- | Packages holding no modules at all, such as the runtime system.
modulelessIn :: Installed -> Set Text
modulelessIn installed =
  Set.fromList [ipName p | p <- installedPackages installed, null (ipModules p)]
