{-# LANGUAGE LambdaCase #-}

-- | Turning the compiler's positions into ours.
module Tilia.Span.Ghc
  ( spanOfReal,
    spanOfSrcSpan,
    spanOf,
    spansOf,
    bracketsSpan,
    bracketSpans,
    outerBracketSpans,
    parenSpans,
    pairSpans,
    inSourceOrder,
  )
where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (listToMaybe, mapMaybe)
import GHC.Parser.Annotation
  ( AnnListBrackets (..),
    AnnParen (..),
    HasLoc,
    getHasLoc,
  )
import GHC.Types.SrcLoc (GenLocated, getLoc, leftmost_smallest)
import GHC.Types.SrcLoc qualified as GHC

import Tilia.Span (Span, mkSpan)

-- | Convert a span the compiler knows to be real.
spanOfReal :: GHC.RealSrcSpan -> Span
spanOfReal s =
  mkSpan
    (GHC.srcSpanStartLine s, GHC.srcSpanStartCol s)
    (GHC.srcSpanEndLine s, GHC.srcSpanEndCol s)

-- | Convert a span that may not be real.
spanOfSrcSpan :: GHC.SrcSpan -> Maybe Span
spanOfSrcSpan = fmap spanOfReal . GHC.srcSpanToRealSrcSpan

-- | Where something the compiler located was written: a node, a token or
-- an annotation.
spanOf :: HasLoc l => l -> Maybe Span
spanOf = spanOfSrcSpan . getHasLoc

-- | Where a list's brackets were written, from the opening one to the
-- closing one.
bracketsSpan :: AnnListBrackets -> Maybe Span
bracketsSpan = uncurry (<>) . bracketSpans

-- | Where each of a list's brackets was written.
bracketSpans ::
  -- | The brackets.
  AnnListBrackets ->
  (Maybe Span, Maybe Span)
bracketSpans = \case
  ListParens open close -> (spanOf open, spanOf close)
  ListBraces open close -> (spanOf open, spanOf close)
  ListSquare open close -> (spanOf open, spanOf close)
  ListBanana open close -> (spanOf open, spanOf close)
  ListNone -> (Nothing, Nothing)

-- | Where each of a pair of parentheses or square brackets was written.
parenSpans ::
  -- | The pair.
  AnnParen ->
  (Maybe Span, Maybe Span)
parenSpans = \case
  AnnParens open close -> (spanOf open, spanOf close)
  AnnParensHash open close -> (spanOf open, spanOf close)
  AnnParensSquare open close -> (spanOf open, spanOf close)

-- | Where each of a pair of brackets written as two tokens was.
pairSpans ::
  (HasLoc open, HasLoc close) =>
  -- | The opening token and the closing one.
  (open, close) ->
  (Maybe Span, Maybe Span)
pairSpans (open, close) = (spanOf open, spanOf close)

-- | Where the outermost of brackets written one pair inside another were.
outerBracketSpans ::
  (HasLoc open, HasLoc close) =>
  -- | The opening brackets, in the order they were written.
  [open] ->
  -- | The closing brackets, in the order they were written.
  [close] ->
  (Maybe Span, Maybe Span)
outerBracketSpans opens closes =
  (spanOf (listToMaybe opens), spanOf (listToMaybe (reverse closes)))

-- | Merge several lists of located things back into the order they were
-- written in.
inSourceOrder :: HasLoc l => [[GenLocated l a]] -> [GenLocated l a]
inSourceOrder = sortBy (leftmost_smallest `on` (getHasLoc . getLoc)) . concat

-- | The span covering every located thing in the list.
spansOf :: HasLoc l => [GenLocated l a] -> Maybe Span
spansOf xs = case mapMaybe spanOf xs of
  [] -> Nothing
  (s : ss) -> Just (foldr (<>) s ss)
