{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The color palette abstraction for printing to color-capable terminals.
module Tilia.Palette
  ( Palette (..),
    paletteFor,
    Color (..),
    paint,
    marker,
  )
where

import Data.Maybe (isJust)
import Data.Text (Text)
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stdout)

-- | Whether to color the output.
data Palette = Colors | Plain
  deriving (Eq, Show)

-- | Color output when there is somebody there to see it.
paletteFor :: IO Palette
paletteFor = do
  refused <- lookupEnv "NO_COLOR"
  terminal <- hIsTerminalDevice stdout
  pure $
    if terminal && not (isJust refused)
      then Colors
      else Plain

-- | Different kinds of colors, classified semantically.
data Color
  = -- | A diff's hunk headings and its asides.
    Meta
  | -- | A line a change removes.
    Gone
  | -- | A line a change adds.
    New
  | -- | A line a change leaves alone.
    Kept
  | -- | Something that went well.
    Good
  | -- | Something that did not, without being wrong.
    Middling
  | -- | Something wrong.
    Bad
  | -- | An operator.
    Operator
  | -- | Somewhere a message points at—a file, a module, the heading over a
    -- file's diff.
    Place
  | -- | A heading over what follows it.
    Header Color
  deriving (Eq, Show)

-- | Color one piece of text, and only that piece.
paint :: Palette -> Color -> Text -> Text
paint Plain _ t = t
paint Colors color t = code color <> t <> "\ESC[0m"
  where
    code = \case
      Meta -> "\ESC[36m"
      Gone -> "\ESC[31m"
      New -> "\ESC[32m"
      Kept -> "\ESC[39m"
      Good -> "\ESC[32m"
      Middling -> "\ESC[33m"
      Bad -> "\ESC[31m"
      Operator -> "\ESC[36m"
      Place -> "\ESC[1m"
      Header i -> "\ESC[1m" <> code i

-- | A marker in brackets, as the summary lines wear one.
marker :: Palette -> Color -> Text -> Text
marker palette color mark = "[" <> paint palette color mark <> "]"
