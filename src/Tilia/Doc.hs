{-# LANGUAGE LambdaCase #-}

-- | Documents: what they hold, and turning them into source text.
module Tilia.Doc
  ( Doc (..),
    Nesting (..),
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
    Nesting (..),
    Spill (..),
    TrailingWhitespace (..),
    Wrapper (..),
    conditionalRange,
    layoutInside,
    printDoc,
    printsNothing,
    unwrap,
    wrap,
  )
import Tilia.Span (spanEndLine, spanStartLine)

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
  DVariant _ d -> f d
  DCppChoice _ bs e -> foldr ((<>) . f . snd) (f e) bs
  d -> maybe mempty (f . snd) (unwrap d)

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
  DVariant a b -> DVariant (f a) (f b)
  DCppChoice cs bs e -> DCppChoice cs [(g, f d) | (g, d) <- bs] (f e)
  d -> maybe d (\(w, x) -> wrap w (f x)) (unwrap d)

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
