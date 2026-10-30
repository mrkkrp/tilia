{-# LANGUAGE LambdaCase #-}

-- | Turning the compiler's positions into ours.
module Tilia.Span.Ghc
  ( spanOfReal,
    spanOfSrcSpan,
    spanOf,
    spansOf,
    bracketsSpan,
    inSourceOrder,
  )
where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import GHC.Parser.Annotation (AnnListBrackets (..), HasLoc, getHasLoc)
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
spanOf :: (HasLoc l) => l -> Maybe Span
spanOf = spanOfSrcSpan . getHasLoc

-- | Where a list's brackets were written, from the opening one to the
-- closing one.
bracketsSpan :: AnnListBrackets -> Maybe Span
bracketsSpan = \case
  ListParens open close -> spanOf open <> spanOf close
  ListBraces open close -> spanOf open <> spanOf close
  ListSquare open close -> spanOf open <> spanOf close
  _ -> Nothing

-- | Merge several lists of located things back into the order they were
-- written in.
inSourceOrder :: (HasLoc l) => [[GenLocated l a]] -> [GenLocated l a]
inSourceOrder = sortBy (leftmost_smallest `on` (getHasLoc . getLoc)) . concat

-- | The span covering every located thing in the list.
spansOf :: (HasLoc l) => [GenLocated l a] -> Maybe Span
spansOf xs = case mapMaybe spanOf xs of
  [] -> Nothing
  (s : ss) -> Just (foldr (<>) s ss)
