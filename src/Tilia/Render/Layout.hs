{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Runs of things: statements, bindings, equations, list elements.
module Tilia.Render.Layout
  ( Bracing (..),
    items,
    itemsSepBy,
    keepBlanks,
    Place (..),
    places,
  )
where

import Data.Choice (Choice, isTrue, pattern Without)
import Tilia.Doc.Combinators
import Tilia.Span

-- | May a block put braces around itself when it is laid out on one line?
data Bracing
  = MayBrace
  | NoBrace
  deriving (Eq, Show)

-- | A block: one item per line when broken, semicolons when flat.
items :: Bracing -> [Doc] -> Doc
items = itemsSepBy (Without #semisWhenBroken)

-- | 'items', with control over whether the broken form carries semicolons
-- too.
itemsSepBy ::
  -- | Semicolons in the broken layout as well?
  Choice "semisWhenBroken" ->
  Bracing ->
  [Doc] ->
  Doc
itemsSepBy semisWhenBroken bracing xs = variant flatForm brokenForm
  where
    flatForm = case (bracing, xs) of
      (MayBrace, []) -> txt "{}"
      (NoBrace, []) -> mempty
      (MayBrace, _) -> txt "{" <> space <> joined <> space <> txt "}"
      (NoBrace, _) -> joined
    joined = sepBy (semi <> space) xs
    brokenForm =
      sepBy (includeWhen (isTrue semisWhenBroken) semi <> hardBreak) xs

-- | Put back the empty lines the author left between items.
keepBlanks ::
  -- | Was there an empty line between two items?
  (Maybe Span -> Maybe Span -> Bool) ->
  -- | Where each item was, and what it prints as.
  [(Maybe Span, Doc)] ->
  [Doc]
keepBlanks blank xs = zipWith gap (Nothing : fmap fst xs) xs
  where
    gap previous (here, d) = includeWhen (blank previous here) hardBreak <> d

-- | Where an item sits among its siblings.
data Place
  = Only
  | First
  | Middle
  | Last
  deriving (Eq, Show)

-- | Label each item of a list with its position.
places :: [a] -> [(Place, a)]
places [] = []
places [x] = [(Only, x)]
places (x : xs) = (First, x) : go xs
  where
    go [] = []
    go [y] = [(Last, y)]
    go (y : ys) = (Middle, y) : go ys
