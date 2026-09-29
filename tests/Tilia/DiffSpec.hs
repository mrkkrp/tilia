{-# LANGUAGE OverloadedStrings #-}

-- | Finding what changed between two texts, and showing it.
module Tilia.DiffSpec (spec) where

import Data.Array (listArray, (!))
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.QuickCheck
import Tilia.Diff
import Tilia.Palette (Palette (..))

spec :: Spec
spec = do
  describe "an edit script" $ do
    it "takes the first sequence to the second" $
      property $ \(Lines xs) (Lines ys) ->
        followed (editScript xs ys) xs ys `shouldBe` True

    it "keeps as many lines as the two have in common" $
      property $ \(Lines xs) (Lines ys) ->
        length (filter (== Context) (editScript xs ys)) `shouldBe` common xs ys

    it "keeps everything when nothing changed" $
      editScript ["a", "b", "c"] ["a", "b", "c"] `shouldBe` [Context, Context, Context]

    it "takes everything out and puts everything in when nothing is shared" $
      editScript ["a", "b"] ["c"] `shouldBe` [Removed, Removed, Added]

  describe "a unified diff" $ do
    it "shows a changed line with the lines around it" $
      diffInFull Plain "A.hs" "a\nb\nc\nd\ne\nf\ng\nh\n" "a\nb\nc\nd\nE\nf\ng\nh\n"
        `shouldBe` T.intercalate
          "\n"
          [ "diff --git a/A.hs b/A.hs",
            "--- a/A.hs",
            "+++ b/A.hs",
            "@@ -2,7 +2,7 @@",
            " b",
            " c",
            " d",
            "-e",
            "+E",
            " f",
            " g",
            " h"
          ]

    it "keeps changes far apart in hunks of their own" $
      length (filter (T.isPrefixOf "@@") (T.lines (diff Plain ("x", "y") far (T.replace "k" "K" (T.replace "b" "B" far)))))
        `shouldBe` 2
  where
    far = T.unlines ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"]

-- | A short run of lines out of a handful, so that two of them have much
-- in common.
newtype Lines = Lines [Text]
  deriving (Show)

instance Arbitrary Lines where
  arbitrary = Lines <$> resize 30 (listOf (elements ["a", "b", "c", "d"]))
  shrink (Lines ls) = Lines <$> shrinkList (const []) ls

-- | Whether following the script consumes both sequences exactly, keeping
-- only lines they share.
followed :: [Mark] -> [Text] -> [Text] -> Bool
followed [] [] [] = True
followed (Context : ms) (x : xs) (y : ys) = x == y && followed ms xs ys
followed (Removed : ms) (_ : xs) ys = followed ms xs ys
followed (Added : ms) xs (_ : ys) = followed ms xs ys
followed _ _ _ = False

-- | The length of the longest common subsequence.
common :: [Text] -> [Text] -> Int
common xs ys = table ! (0, 0)
  where
    n = length xs
    m = length ys
    xa = listArray (0, n - 1) xs
    ya = listArray (0, m - 1) ys
    table =
      listArray
        ((0, 0), (n, m))
        [cell i j | i <- [0 .. n], j <- [0 .. m]]
    cell i j
      | i == n || j == m = 0
      | xa ! i == ya ! j = 1 + table ! (i + 1, j + 1)
      | otherwise = max (table ! (i + 1, j)) (table ! (i, j + 1))
