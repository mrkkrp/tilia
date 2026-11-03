{-# LANGUAGE LambdaCase #-}

-- | Documents: what they hold, and turning them into source text.
module Tilia.Doc
  ( Doc (..),
    Spill (..),
    Conditional (..),
    conditionalRange,
    LineStart (..),
    TrailingWhitespace (..),
    Layout (..),
    printDoc,
    printsNothing,
    printsOnOneLine,
    foldChildren,
    mapChildren,
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

import Tilia.Doc.Internal
  ( Conditional (..),
    Doc (..),
    Layout (..),
    LineStart (..),
    Spill (..),
    TrailingWhitespace (..),
    conditionalRange,
    printDoc,
    printsNothing,
  )
import Tilia.Span (Span, spanEndLine, spanStartLine)

-- | Does this document print on one line, holding neither a line break nor
-- a choice?
printsOnOneLine ::
  -- | The layout of the group it is printed in.
  Layout ->
  -- | The document.
  Doc ->
  Bool
printsOnOneLine layout = all single . spineAt layout
  where
    single x = case unwrap x of
      Just (w, y) -> printsOnOneLine (layoutInside layout w) y
      Nothing -> case x of
        DHardBreak -> False
        DBreak -> layout /= Broken
        DSoftBreak -> layout /= Broken
        DVerbatimBreak{} -> False
        DCppChoice{} -> False
        _ -> True

-- | Combine what a function makes of each document directly inside this
-- one, a variant's broken layout standing for the variant.
foldChildren ::
  (Monoid m) =>
  -- | What to make of each document inside.
  (Doc -> m) ->
  -- | The document.
  Doc ->
  m
foldChildren f = \case
  DCat a b -> f a <> f b
  DNest _ d -> f d
  DAlign d -> f d
  DCppMarginNote d -> f d
  DGroup _ d -> f d
  DVariant _ d -> f d
  DLocated _ d -> f d
  DFence _ d -> f d
  DCppChoice _ bs e -> foldr ((<>) . f . snd) (f e) bs
  _ -> mempty

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
  DNest n d -> DNest n (f d)
  DAlign d -> DAlign (f d)
  DCppMarginNote d -> DCppMarginNote (f d)
  DGroup l d -> DGroup l (f d)
  DVariant a b -> DVariant (f a) (f b)
  DLocated s d -> DLocated s (f d)
  DFence s d -> DFence s (f d)
  DCppChoice cs bs e -> DCppChoice cs [(g, f d) | (g, d) <- bs] (f e)
  d -> d

-- | A document as the sequence of things it concatenates.
spine ::
  -- | The document.
  Doc ->
  [Doc]
spine = \case
  DEmpty -> []
  DCat a b -> spine a <> spine b
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
  DCat a b -> spineAt layout a <> spineAt layout b
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

-- | A node holding exactly one document, less the document.
data Wrapper
  = WLocated !Span
  | WFence !Span
  | WNest !Int
  | WAlign
  | WCppMarginNote
  | WGroup !Layout
  deriving (Eq, Show)

-- | A document as the wrapper it is and the document it holds.
unwrap ::
  -- | The document.
  Doc ->
  Maybe (Wrapper, Doc)
unwrap = \case
  DLocated s d -> Just (WLocated s, d)
  DFence s d -> Just (WFence s, d)
  DNest n d -> Just (WNest n, d)
  DAlign d -> Just (WAlign, d)
  DCppMarginNote d -> Just (WCppMarginNote, d)
  DGroup l d -> Just (WGroup l, d)
  _ -> Nothing
{-# INLINE unwrap #-}

-- | Put a document in a wrapper.
wrap ::
  -- | The wrapper.
  Wrapper ->
  -- | The document to put in it.
  Doc ->
  Doc
wrap = \case
  WLocated s -> DLocated s
  WFence s -> DFence s
  WNest n -> DNest n
  WAlign -> DAlign
  WCppMarginNote -> DCppMarginNote
  WGroup l -> DGroup l

-- | The layout inside a wrapper laid out like this.
layoutInside ::
  -- | The layout of the group the wrapper is printed in.
  Layout ->
  -- | The wrapper.
  Wrapper ->
  Layout
layoutInside layout = \case
  WGroup l -> l
  _ -> layout
