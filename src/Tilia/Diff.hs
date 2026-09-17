{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Showing what changed.
module Tilia.Diff
  ( diff,
    diffInFull,
  )
where

import Data.Algorithm.Diff qualified as D
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Tilia.Palette (Color (..), Palette, paint)

-- | One line of the comparison, with the number it has on each side.
data Line = Line !Mark !Int !Int !Text

-- | The type of mark.
data Mark = Context | Removed | Added
  deriving (Eq)

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
    lines' = tag (D.getGroupedDiff (split before) (split after))
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

-- | Number the lines of a grouped diff on both sides at once.
tag :: [D.Diff [Text]] -> [Line]
tag = go 1 1
  where
    go _ _ [] = []
    go !b !a (d : ds) = case d of
      D.Both xs _ ->
        [Line Context (b + i) (a + i) x | (i, x) <- zip [0 ..] xs]
          <> go (b + length xs) (a + length xs) ds
      D.First xs ->
        [Line Removed (b + i) a x | (i, x) <- zip [0 ..] xs]
          <> go (b + length xs) a ds
      D.Second xs ->
        [Line Added b (a + i) x | (i, x) <- zip [0 ..] xs]
          <> go b (a + length xs) ds
