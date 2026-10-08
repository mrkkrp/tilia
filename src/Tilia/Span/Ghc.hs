{-# LANGUAGE LambdaCase #-}

-- | Turning the compiler's positions into ours.
module Tilia.Span.Ghc
  ( spanOfReal,
    spanOfSrcSpan,
    spanOf,
    spansOf,
    tokenSpan,
    uniTokenSpan,
    annSpan,
    bracketsSpan,
  )
where

import Data.Maybe (mapMaybe)
import GHC.Parser.Annotation
  ( AnnListBrackets (..),
    EpToken,
    EpUniToken (..),
    HasLoc,
    getEpTokenSrcSpan,
    getHasLoc,
  )
import GHC.Types.SrcLoc (GenLocated)
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

-- | The span of a located thing.
spanOf :: (HasLoc l) => GenLocated l a -> Maybe Span
spanOf = spanOfSrcSpan . getHasLoc

-- | Where a keyword or a piece of punctuation was written.
tokenSpan :: EpToken sym -> Maybe Span
tokenSpan = spanOfSrcSpan . getEpTokenSrcSpan

-- | Where a keyword or a piece of punctuation that has a Unicode spelling
-- was written.
uniTokenSpan :: EpUniToken tok utok -> Maybe Span
uniTokenSpan = \case
  EpUniTok l _ -> annSpan l
  NoEpUniTok -> Nothing

-- | Where an annotation says something was written.
annSpan :: (HasLoc l) => l -> Maybe Span
annSpan = spanOfSrcSpan . getHasLoc

-- | Where a list's brackets were written, from the opening one to the
-- closing one.
bracketsSpan :: AnnListBrackets -> Maybe Span
bracketsSpan = \case
  ListParens open close -> tokenSpan open <> tokenSpan close
  ListBraces open close -> tokenSpan open <> tokenSpan close
  ListSquare open close -> tokenSpan open <> tokenSpan close
  _ -> Nothing

-- | The span covering every located thing in the list.
spansOf :: (HasLoc l) => [GenLocated l a] -> Maybe Span
spansOf xs = case mapMaybe spanOf xs of
  [] -> Nothing
  (s : ss) -> Just (foldr (<>) s ss)
