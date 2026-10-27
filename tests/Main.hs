{-# LANGUAGE LambdaCase #-}

-- | Running the suite, or the share of it a run is for.
--
-- On CI all tests are divided in two classes: those that exercise something
-- about the compiler and those that do not. The first group is run on every
-- compiler\/shard, the second group is split up between compilers\/shards.
--
-- Set @TILIA_SHARD@ to @i/n@ — @1/3@, @2/3@, @3/3@ — to take the @i@th
-- share of @n@. Unset, which is what @cabal test@ gives you, runs
-- everything.
module Main (main) where

import Data.List (intercalate, isPrefixOf)
import Spec qualified
import System.Environment (lookupEnv)
import Test.Hspec.Runner (Config (..), Path, defaultConfig, hspecWith)
import Text.Read (readMaybe)
import Tilia.TestUtils (fingerprint)

main :: IO ()
main =
  shardFrom <$> lookupEnv "TILIA_SHARD" >>= \case
    Everything -> Spec.main
    Share i n ->
      hspecWith defaultConfig{configFilterPredicate = Just (taking i n)} Spec.spec

-- | Which share of the suite a run is for.
data Shard
  = Everything
  | -- | The @i@th share of @n@, counting from one.
    Share Int Int

-- | Read a share, or take the whole suite when nothing sensible is asked
-- for. Being wrong here runs too much rather than too little.
shardFrom :: Maybe String -> Shard
shardFrom = \case
  Just asked
    | (i, '/' : n) <- span (/= '/') asked,
      Just i' <- readMaybe i,
      Just n' <- readMaybe n,
      n' > 0,
      i' >= 1,
      i' <= n' ->
        Share i' n'
  _ -> Everything

-- | Which tests the @i@th share of @n@ takes: every one that leans on the
-- compiler, and its own share of the rest.
taking :: Int -> Int -> Path -> Bool
taking i n path = leansOnCompiler path || share path == i - 1
  where
    share = fromIntegral . (`mod` fromIntegral n) . fingerprint . spell

-- | A test's path, written out, so that a share is decided by what the test
-- is rather than by where it happens to fall in the order.
spell :: Path -> String
spell (groups, requirement) = intercalate "/" (groups <> [requirement])

-- | Is this test one whose answer the compiler can change?
leansOnCompiler :: Path -> Bool
leansOnCompiler (groups, _) = case groups of
  group : _ -> any (`isPrefixOf` group) compilerBound
  [] -> False

-- | The groups that ask the compiler, or its package database, or the plan
-- it solved, and could therefore fail on one compiler and pass on another.
compilerBound :: [String]
compilerBound =
  [ "Tilia.Fixity.Dependencies",
    "Tilia.Fixity.HiFile",
    "Tilia.Fixity.PackageDb",
    "Tilia.Fixity.Plan"
  ]
