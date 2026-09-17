{-# LANGUAGE OverloadedStrings #-}

-- | Runs of things: statements, bindings, equations, list elements.
module Tilia.Render.Layout
  ( -- * Blocks
    Bracing (..),
    items,
    itemsSepBy,

    -- * Blank lines
    keepBlanks,

    -- * Positions in a run
    Place (..),
    places,
  )
where

import Tilia.Doc.Combinators
import Tilia.Span

----------------------------------------------------------------------------
-- Blocks

-- | May a block put braces around itself when it is laid out on one line?
--
-- It may not when something outside it is already doing so: nested braces
-- would be correct but unreadable, and more to the point the outer block
-- has already made the items unambiguous.
data Bracing
  = MayBrace
  | NoBrace
  deriving (Eq, Show)

-- | A block: one item per line when broken, semicolons when flat.
items :: Bracing -> [Doc] -> Doc
items = itemsSepBy False

-- | 'items', with control over whether the broken form carries semicolons
-- too.
--
-- It has to when the block is standing in for something that would
-- otherwise be read as continuing: an or-pattern inside an as-pattern, for
-- instance, where a bare line break would let the next alternative be taken
-- for a new argument.
itemsSepBy ::
  -- | Semicolons in the broken layout as well?
  Bool ->
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
      sepBy (includeWhen semisWhenBroken semi <> hardBreak) xs

----------------------------------------------------------------------------
-- Blank lines

-- | Put back the empty lines the author left between items.
keepBlanks ::
  -- | Was there an empty line between two items?
  (Maybe Span -> Maybe Span -> Bool) ->
  -- | Where each item was, and what it prints as.
  [(Maybe Span, Doc)] ->
  [Doc]
keepBlanks blank xs = zipWith gap (Nothing : map fst xs) xs
  where
    gap previous (here, d) = includeWhen (blank previous here) hardBreak <> d

----------------------------------------------------------------------------
-- Positions in a run

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
