{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Documents: what they hold, and turning them into source text.
module Tilia.Doc
  ( Doc (..),
    SeparatorSide (..),
    Spill (..),
    LineStart (..),
    TrailingWhitespace (..),
    Layout (..),
    Conditional (..),
    conditionalRange,
    printDoc,
    separatorSide,
    printsNothing,
    namesOnly,
    referentIn,
    pattern Cat,
    printsOnOneLine,
    choosing,
    foldChildren,
    mapChildren,
    regionsIn,
    foldRegions,
    regionOf,
    spine,
    spineAt,
    printedFrom,
    onlySpacing,
    onlyBreaks,
    Wrapper (..),
    unwrap,
    wrap,
    layoutInside,
  )
where

import Data.Maybe (mapMaybe)
import Data.Monoid (Any (..))

import Tilia.Doc.Internal
  ( Conditional (..),
    Doc (..),
    Layout (..),
    LineStart (..),
    SeparatorSide (..),
    Spill (..),
    TrailingWhitespace (..),
    Wrapper (..),
    conditionalRange,
    layoutInside,
    namesOnly,
    printDoc,
    printsNothing,
    referentIn,
    unwrap,
    wrap,
  )
import Tilia.Span (Span, spanEndLine, spanStartLine)

-- | The side a separator, in the indentation it was written in or named as
-- a referent, is written on.
separatorSide ::
  -- | The document.
  Doc ->
  Maybe SeparatorSide
separatorSide = \case
  DSeparator s _ _ -> Just s
  DIndented _ _ d -> separatorSide d
  DReferent _ d -> separatorSide d
  _ -> Nothing

-- | Does this document print on one line, holding neither a line break nor
-- a choice?
printsOnOneLine ::
  -- | The layout of the group it is printed in.
  Layout ->
  -- | The document.
  Doc ->
  Bool
printsOnOneLine layout = \case
  DCat a b -> printsOnOneLine layout a && printsOnOneLine layout b
  DIndented _ a b -> printsOnOneLine layout a && printsOnOneLine layout b
  DVariant flatD brokenD ->
    printsOnOneLine layout (case layout of Broken -> brokenD; _ -> flatD)
  DHardBreak -> False
  DBreak -> layout /= Broken
  DSoftBreak -> layout /= Broken
  DVerbatimBreak{} -> False
  DCppChoice{} -> False
  x ->
    maybe True (\(w, y) -> printsOnOneLine (layoutInside layout w) y) (unwrap x)

-- | Is there a choice anywhere in this document?
choosing ::
  -- | The document.
  Doc ->
  Bool
choosing = getAny . go
  where
    go = \case
      DCppChoice{} -> Any True
      d -> foldChildren go d

-- | Combine what a function makes of each document directly inside this
-- one, a variant's broken layout standing for the variant.
foldChildren ::
  Monoid m =>
  -- | What to make of each document inside.
  (Doc -> m) ->
  -- | The document.
  Doc ->
  m
foldChildren f = \case
  DCat a b -> f a <> f b
  DIndented _ a b | not (namesOnly a) -> f a <> f b
  DVariant _ d -> f d
  DCppChoice _ bs e -> foldr ((<>) . f . snd) (f e) bs
  d -> maybe mempty (f . snd) (unwrap d)
{-# INLINE foldChildren #-}

-- | Rewrite the documents directly inside this one, both layouts of a
-- variant included.
mapChildren ::
  -- | How to rewrite each document inside.
  (Doc -> Doc) ->
  -- | The document.
  Doc ->
  Doc
mapChildren f = \case
  DCat a b -> f a <> f b
  DIndented n a b | not (namesOnly a) -> DIndented n (f a) (f b)
  DVariant a b -> DVariant (f a) (f b)
  DCppChoice cs bs e -> DCppChoice cs [(g, f d) | (g, d) <- bs] (f e)
  d -> maybe d (\(w, x) -> wrap w (f x)) (unwrap d)
{-# INLINE mapChildren #-}

-- | Every region a document records, and what it holds.
regionsIn ::
  -- | The document.
  Doc ->
  [(Span, Doc)]
regionsIn = foldRegions (\s d inside -> (s, d) : inside)

-- | Fold the regions a document records, giving each its span, what it
-- holds, and what the fold made of the regions inside it.
foldRegions ::
  Monoid m =>
  -- | What to make of a region, given what was made of those inside it.
  (Span -> Doc -> m -> m) ->
  -- | The document.
  Doc ->
  m
foldRegions f = go
  where
    go = \case
      DLocated s d -> f s d (go d)
      DFence s d -> f s d (go d)
      d -> foldChildren go d
{-# INLINE foldRegions #-}

-- | The smallest span covering the outermost regions a document records.
regionOf ::
  -- | The document.
  Doc ->
  Maybe Span
regionOf = \case
  DLocated s _ -> Just s
  DFence s _ -> Just s
  d -> foldChildren regionOf d

-- | A document that prints one document and then another: a
-- concatenation, or a relation printing the part it measures from, as that
-- part and the relation left with only the name of its referent.
pattern Cat :: Doc -> Doc -> Doc
pattern Cat a b <- (halves -> Just (a, b))

-- | The two documents a document prints one after the other, if it does.
halves ::
  -- | The document.
  Doc ->
  Maybe (Doc, Doc)
halves = \case
  DCat a b -> Just (a, b)
  DIndented n a b
    | not (namesOnly a) ->
        Just
          (a, DIndented n (maybe mempty (`DReferent` mempty) (referentIn a)) b)
  _ -> Nothing

-- | A document as the sequence of things it concatenates.
spine ::
  -- | The document.
  Doc ->
  [Doc]
spine = \case
  DEmpty -> []
  Cat a b -> spine a <> spine b
  d -> [d]

-- | 'spine', with the variants resolved the way this layout will print them.
spineAt ::
  -- | The layout of the group it is printed in.
  Layout ->
  -- | The document.
  Doc ->
  [Doc]
spineAt layout = \case
  DEmpty -> []
  Cat a b -> spineAt layout a <> spineAt layout b
  DVariant flatD brokenD ->
    spineAt layout (case layout of Broken -> brokenD; _ -> flatD)
  d -> [d]

-- | The lines of the input a document holds something printed from, as far
-- down as there is anything.
printedFrom ::
  -- | The document.
  Doc ->
  [(Int, Int)]
printedFrom = \case
  DLocated s x -> (spanStartLine s, spanEndLine s) : printedFrom x
  DFence s x -> (spanStartLine s, spanEndLine s) : printedFrom x
  d@(DCppChoice cs _ _) ->
    mapMaybe conditionalRange cs <> foldChildren printedFrom d
  d -> foldChildren printedFrom d

-- | Nothing but the whitespace that separates one thing from the next.
onlySpacing ::
  -- | The document.
  Doc ->
  Bool
onlySpacing d = d == DSpace || onlyBreaks d

-- | Nothing but what ends a line, in a broken group at least.
onlyBreaks ::
  -- | The document.
  Doc ->
  Bool
onlyBreaks = \case
  DEmpty -> True
  DBreak -> True
  DSoftBreak -> True
  DHardBreak -> True
  DCloseLine -> True
  DCloseLineUnlessAfterOpener _ -> True
  _ -> False
