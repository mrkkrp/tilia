{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Showing what changed.
module Tilia.Diff
  ( diff,
    diffInFull,
    Mark (..),
    editScript,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.Array.Unboxed (Array, UArray, listArray, (!))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Tilia.Palette (Color (..), Palette, paint)

-- | What a line of the comparison does.
data Mark = Context | Removed | Added
  deriving (Eq, Show)

-- | A unified diff of two texts.
diff ::
  Palette ->
  -- | What to call the two sides.
  (Text, Text) ->
  -- | Before.
  Text ->
  -- | After.
  Text ->
  Text
diff palette = unified palette (Just roomFor) []

-- | The whole of a unified diff of one file against its formatted self,
-- headed the way @git diff@ heads one.
diffInFull ::
  Palette ->
  -- | The file, named as it was given on the command line.
  FilePath ->
  -- | What is in it.
  Text ->
  -- | What would be.
  Text ->
  Text
diffInFull palette path =
  unified
    palette
    Nothing
    [paint palette Place ("diff --git " <> before <> " " <> after)]
    (before, after)
  where
    before = "a/" <> T.pack path
    after = "b/" <> T.pack path

-- | One line of the comparison, with the number it has on each side.
data Line = Line !Mark !Int !Int !Text

-- | A unified diff, with whatever heading and limit the caller wants.
unified ::
  Palette ->
  -- | How many lines are worth printing, where there is a limit at all.
  Maybe Int ->
  -- | Whatever goes above the two file names.
  [Text] ->
  -- | What to call the two sides.
  (Text, Text) ->
  -- | Before.
  Text ->
  -- | After.
  Text ->
  Text
unified palette limit above (beforeName, afterName) before after
  | null hunks,
    before /= after =
      "(the two differ only in how they end their lines)"
  | null hunks =
      "(the two are identical as text, so the difference is in something\
      \ the text does not show)"
  | otherwise = T.intercalate "\n" (above <> heading <> shown)
  where
    heading =
      [ paint palette (Header Gone) ("--- " <> beforeName),
        paint palette (Header New) ("+++ " <> afterName)
      ]

    shown = case limit of
      Just room | length body > room -> take room body <> [omitted room]
      _ -> body
      where
        omitted room =
          paint palette Meta $
            "… and " <> T.pack (show (length body - room)) <> " more lines"

    body = concatMap render hunks

    render range = hunkHeading range : fmap line (slice range)

    hunkHeading range =
      paint palette Meta $
        "@@ -"
          <> span' beforeOf (countingBefore (slice range))
          <> " +"
          <> span' afterOf (countingAfter (slice range))
          <> " @@"
      where
        span' which n = case slice range of
          (l : _) -> T.pack (show (which l)) <> "," <> T.pack (show n)
          [] -> "0,0"

    line (Line mark _ _ text) = case mark of
      Context -> paint palette Unchanged (" " <> text)
      Removed -> paint palette Gone ("-" <> text)
      Added -> paint palette New ("+" <> text)

    slice (from, to) = take (to - from + 1) (drop from lines')

    countingBefore = length . filter (\(Line m _ _ _) -> m /= Added)
    countingAfter = length . filter (\(Line m _ _ _) -> m /= Removed)
    beforeOf (Line _ b _ _) = b
    afterOf (Line _ _ a _) = a

    hunks = merge [(max 0 (i - margin), min (total - 1) (i + margin)) | i <- changed]
    changed = [i | (i, Line m _ _ _) <- zip [0 ..] lines', m /= Context]
    total = length lines'
    lines' = tag (split before) (split after)
    split = fmap withoutReturn . T.splitOn "\n"
    withoutReturn l = fromMaybe l (T.stripSuffix "\r" l)

-- | How many unchanged lines to show either side of a change.
margin :: Int
margin = 3

-- | How many lines of diff are worth printing before it stops being read.
roomFor :: Int
roomFor = 60

-- | Join hunks that have grown into one another.
merge :: [(Int, Int)] -> [(Int, Int)]
merge = \case
  ((a, b) : (c, d) : rest)
    | c <= b + 1 -> merge ((a, max b d) : rest)
    | otherwise -> (a, b) : merge ((c, d) : rest)
  xs -> xs

-- | Number the lines of a comparison on both sides at once.
tag :: [Text] -> [Text] -> [Line]
tag xs ys = go 1 1 (editScript xs ys)
  where
    before = listArray (1, length xs) xs :: Array Int Text
    after = listArray (1, length ys) ys :: Array Int Text
    go !b !a = \case
      [] -> []
      Context : ms -> Line Context b a (before ! b) : go (b + 1) (a + 1) ms
      Removed : ms -> Line Removed b a (before ! b) : go (b + 1) a ms
      Added : ms -> Line Added b a (after ! a) : go b (a + 1) ms

-- | A shortest edit script turning one sequence of lines into another, as
-- what each line of the comparison does, in order.
--
-- Myers' O(ND) algorithm in linear space: the middle of an edit script is
-- found by searching from both ends at once, and either side of it is
-- solved the same way.
editScript :: [Text] -> [Text] -> [Mark]
editScript xs ys = go 0 n 0 m []
  where
    n = length xs
    m = length ys
    (as, bs) = interned xs ys
    go aLo aHi bLo bHi rest
      | aLo < aHi,
        bLo < bHi,
        as ! aLo == bs ! bLo =
          Context : go (aLo + 1) aHi (bLo + 1) bHi rest
      | aLo < aHi,
        bLo < bHi,
        as ! (aHi - 1) == bs ! (bHi - 1) =
          go aLo (aHi - 1) bLo (bHi - 1) (Context : rest)
      | aLo == aHi = replicate (bHi - bLo) Added <> rest
      | bLo == bHi = replicate (aHi - aLo) Removed <> rest
      | Just (x, y) <- middle as bs (aLo, aHi) (bLo, bHi),
        (x, y) /= (aLo, bLo),
        (x, y) /= (aHi, bHi) =
          go aLo x bLo y (go x aHi y bHi rest)
      | otherwise = replicate (aHi - aLo) Removed <> replicate (bHi - bLo) Added <> rest

-- | Both sequences of lines as numbers, equal where the lines are.
interned :: [Text] -> [Text] -> (UArray Int Int, UArray Int Int)
interned xs ys = (numbered xs, numbered ys)
  where
    numbers = Map.fromList (zip (xs <> ys) [0 ..])
    numbered ls = listArray (0, length ls - 1) (fmap (numbers Map.!) ls)

-- | A point that a shortest edit script between two ranges passes through,
-- found where the searches from their two ends meet, or 'Nothing' if they
-- do not.
--
-- The ranges are expected to differ at both ends.
middle ::
  UArray Int Int ->
  UArray Int Int ->
  (Int, Int) ->
  (Int, Int) ->
  Maybe (Int, Int)
middle as bs (aLo, aHi) (bLo, bHi) = runST $ do
  forth <- newArray (0, size - 1) (-1) :: ST s (STUArray s Int Int)
  back <- newArray (0, size - 1) (-1) :: ST s (STUArray s Int Int)
  writeArray forth (offset + 1) 0
  writeArray back (offset + 1) 0
  let rounds d fs fe bStart bEnd
        | d >= maxD = pure Nothing
        | otherwise =
            forwards d (fs - d) fs fe >>= \case
              Left found -> pure (Just found)
              Right (fs', fe') ->
                backwards d (bStart - d) bStart bEnd >>= \case
                  Left found -> pure (Just found)
                  Right (bStart', bEnd') -> rounds (d + 1) fs' fe' bStart' bEnd'
      forwards d k fs fe
        | k > d - fe = pure (Right (fs, fe))
        | otherwise = do
            x0 <- furthest forth d k
            let x = slide (\i j -> as ! (aLo + i) == bs ! (bLo + j)) x0 (x0 - k)
                y = x - k
            writeArray forth (offset + k) x
            if
              | x > n -> forwards d (k + 2) fs (fe + 2)
              | y > m -> forwards d (k + 2) (fs + 2) fe
              | odd delta -> do
                  let o = offset + delta - k
                  other <-
                    if o >= 0 && o < size
                      then readArray back o
                      else pure (-1)
                  if other /= -1 && x >= n - other
                    then pure (Left (aLo + x, bLo + y))
                    else forwards d (k + 2) fs fe
              | otherwise -> forwards d (k + 2) fs fe
      backwards d k bStart bEnd
        | k > d - bEnd = pure (Right (bStart, bEnd))
        | otherwise = do
            x0 <- furthest back d k
            let x = slide (\i j -> as ! (aHi - 1 - i) == bs ! (bHi - 1 - j)) x0 (x0 - k)
                y = x - k
            writeArray back (offset + k) x
            if
              | x > n -> backwards d (k + 2) bStart (bEnd + 2)
              | y > m -> backwards d (k + 2) (bStart + 2) bEnd
              | even delta -> do
                  let o = offset + delta - k
                  other <- if o >= 0 && o < size then readArray forth o else pure (-1)
                  if other /= -1 && other >= n - x
                    then pure (Left (aLo + other, bLo + other - (delta - k)))
                    else backwards d (k + 2) bStart bEnd
              | otherwise -> backwards d (k + 2) bStart bEnd
  rounds 0 0 0 0 0
  where
    n = aHi - aLo
    m = bHi - bLo
    delta = n - m
    maxD = (n + m + 1) `div` 2
    offset = maxD
    size = 2 * maxD + 2
    slide same x y
      | x < n, y < m, same x y = slide same (x + 1) (y + 1)
      | otherwise = x
    furthest v d k = do
      below <- readArray v (offset + k - 1)
      above <- readArray v (offset + k + 1)
      pure $
        if k == -d || (k /= d && below < above)
          then above
          else below + 1
