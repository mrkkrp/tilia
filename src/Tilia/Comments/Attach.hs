{-# LANGUAGE LambdaCase #-}

-- | Attaching a stream of comments to a 'Doc'.
--
-- Attachment happens once, on the finished document, before anything is
-- rendered. A comment becomes an ordinary part of the document like any
-- other, and from then on nothing distinguishes it.
module Tilia.Comments.Attach
  ( attachComments,
  )
where

import Data.Bifunctor (first, second)
import Data.List (mapAccumL, unsnoc)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Tilia.Comments
import Tilia.Comments.Place
import Tilia.Doc.Combinators
import Tilia.Doc.Internal (Doc (..))
import Tilia.Span

-- | Attache comments to a 'Doc'.
attachComments :: [Comment] -> Doc -> Doc
attachComments cs doc = written <> closingComments (unplaced left)
  where
    (written, left) = walk (placeComments regions fences cs) doc
    (regions, fences) = markedSpans doc

-- | The comments nothing came to collect, written after everything.
closingComments :: [Comment] -> Doc
closingComments = \case
  [] -> mempty
  (opening : rest) -> placeOne True opening <> foldMap (placeOne False) rest
  where
    placeOne opensTheRun c =
      commentDoc c $
        closeLine
          <> includeWhen (opensTheRun || commentGapAbove c) blankLine
          <> commentText c
          <> closeLine

-- | The spans of every 'DLocated' in the document, and of every 'DFence',
-- in that order.
markedSpans :: Doc -> ([Span], [Span])
markedSpans = \case
  DLocated s d -> first (s :) (markedSpans d)
  DFence s d -> second (s :) (markedSpans d)
  DCat a b -> markedSpans a <> markedSpans b
  DNest _ d -> markedSpans d
  DAlign d -> markedSpans d
  DGroup _ d -> markedSpans d
  DVariant a _ -> markedSpans a
  DCppChoice bs e -> foldMap (markedSpans . snd) bs <> markedSpans e
  _ -> ([], [])

-- | Write the placed comments into the document, each one around the region
-- it was given to. Taking is destructive: the 'Placements' is threaded
-- through the walk and a region's comments are removed as they are written,
-- so a span that occurs twice is served once. The two sides of a 'DVariant'
-- are the exception—they are one piece of code laid out two ways, so both
-- are walked from the same 'Placements' and both come out holding the
-- comment, and only one of them is ever printed. What is still unwritten
-- when the walk ends is returned next to the resulting 'Doc'.
walk :: Placements -> Doc -> (Doc, Placements)
walk = go
  where
    go p = \case
      DCat a b ->
        let (a', p') = go p a
            (b', p'') = go p' b
         in (DCat a' b', p'')
      DLocated s d ->
        let (mine, p') = claimPlaced s p
            (d', p'') = go p' d
            write position cs =
              foldMap (writtenAs (isEmptyAnchor s) position) cs
            before' = heldOffFrom d [c | (q, c) <- mine, q == Before]
            after' = [c | (q, c) <- mine, q == After]
         in (write Before before' <> DLocated s d' <> write After after', p'')
      DFence s d -> first (DFence s) (go p d)
      DCppChoice bs e ->
        let branch q (c, d) = let (d', q') = go q d in (q', (c, d'))
            (p', bs') = mapAccumL branch p bs
            (e', p'') = go p' e
         in (DCppChoice bs' e', p'')
      DNest n d -> first (DNest n) (go p d)
      DAlign d -> first DAlign (go p d)
      DGroup l d -> first (DGroup l) (go p d)
      DVariant a b ->
        let (a', p') = go p a
            (b', _) = go p b
         in (DVariant a' b', p')
      d -> (d, p)

-- | Hold the last comment off a Haddock about to be written under it.
--
-- Only a comment written as @--@ lines needs holding off: the lexer would
-- read it and the Haddock under it as one comment. A @{- … -}@ ends at its
-- own bracket and may sit against whatever follows.
heldOffFrom :: Doc -> [Comment] -> [Comment]
heldOffFrom d cs = case unsnoc cs of
  Just (earlier, c)
    | not (bracketed c),
      opensWithHaddock d ->
        earlier <> [c{commentGapBelow = True}]
  _ -> cs

-- | Does this region begin its first line with a Haddock?
opensWithHaddock :: Doc -> Bool
opensWithHaddock = maybe False opensHaddock . listToMaybe . fst . firstLine Broken
  where
    firstLine layout = \case
      DText t -> ([t], False)
      DCat a b -> case firstLine layout a of
        (before, True) -> (before, True)
        (before, False) -> first (before <>) (firstLine layout b)
      DNest _ x -> firstLine layout x
      DAlign x -> firstLine layout x
      DLocated _ x -> firstLine layout x
      DFence _ x -> firstLine layout x
      DGroup l x -> firstLine l x
      DVariant a b -> firstLine layout (case layout of Flat -> a; Broken -> b)
      DHardBreak -> ([], True)
      DCloseLine -> ([], True)
      DBreak -> ([], layout == Broken)
      DSoftBreak -> ([], layout == Broken)
      _ -> ([], False)

-- | Is this region an 'emptyAnchor' rather than one covering something
-- written?
isEmptyAnchor :: Span -> Bool
isEmptyAnchor s = startPoint s == endPoint s

-- | Render one 'Comment' where it was placed.
writtenAs ::
  -- | Does what follows only mark where the construct ends?
  Bool ->
  -- | Comment position.
  Position ->
  -- | The comment to render.
  Comment ->
  Doc
writtenAs atTheEnd position c = commentDoc c $ case shapeOf position c of
  InPlace -> case position of
    Before -> includeWhen (not (commentTrailing c)) space <> body <> space
    After -> space <> body <> space
  EndsTheLine -> space <> body <> closeLine <> gapBelow
  HeldBack -> holdBack (renderComment c)
  OnItsOwnLines -> gapAbove <> closeLine <> body <> closeLine <> gapBelow
  where
    body = commentText c
    gapAbove = includeWhen (commentGapAbove c) (closeLine <> blankLine)
    gapBelow = includeWhen (commentGapBelow c && not atTheEnd) blankLine

-- | A comment, and the spacing that goes with it, as one region.
commentDoc :: Comment -> Doc -> Doc
commentDoc = located . commentSpan

-- | The text of a comment, laid out as it was written.
commentText :: Comment -> Doc
commentText c =
  align $
    sepBy
      (verbatimBreak AtIndent TrimWhitespace)
      (fmap txt (NE.toList (commentBody c)))

-- | Put this text at the end of the line this position falls on.
holdBack :: Text -> Doc
holdBack = DHoldBack

-- | End the current line and leave a mark so that a line break that follows
-- it immediately (if any) will have no effect.
closeLine :: Doc
closeLine = DCloseLine
