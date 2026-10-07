{-# LANGUAGE LambdaCase #-}

-- | Attaching a stream of comments to a 'Doc'.
--
-- Attachment happens once, on the finished document, before anything is
-- rendered. A comment becomes an ordinary part of the document like any
-- other, and from then on nothing distinguishes it.
module Tilia.Comments.Attach
  ( attachComments,
    attachScopedComments,
    Margin (..),
    noMargin,
  )
where

import Control.Applicative ((<|>))
import Data.Bifunctor (first, second)
import Data.List (unsnoc)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Monoid (First (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Tilia.Comments
import Tilia.Comments.Place
import Tilia.Doc.Combinators
import Tilia.Doc.Internal (Doc (..), Spill (..), foldChildren)
import Tilia.Span

-- | Attach comments to a 'Doc'.
attachComments :: [Comment] -> Doc -> Doc
attachComments = attachScopedComments noMargin (const ())

-- | What parts of a comment go at the margin.
data Margin = Margin
  { -- | A line of a comment, as a preprocessor directive written in one has
    -- to: the preprocessor reads directives out of comments as well.
    marginLine :: Text -> Bool,
    -- | A comment that goes to the margin when it comes out right above a
    -- preprocessor directive, which it then documents.
    marginComment :: Comment -> Bool
  }

-- | Nothing at the margin that the indentation does not put there.
noMargin :: Margin
noMargin = Margin (const False) (const False)

-- | Attach comments to a 'Doc', each only to a region in the same scope as
-- itself.
--
-- A scope is a part of the input a comment may not be carried out of, such
-- as one branch of a conditional: a comment written in a branch has to stay
-- among what that branch holds, and one written outside it must not end up
-- inside. Placement never looks past a comment's own scope, so where it
-- goes is decided among the regions it could be printed next to in every
-- configuration it is in.
attachScopedComments ::
  (Ord k) =>
  -- | What goes at the margin.
  Margin ->
  -- | The scope of what was written at a span.
  (Span -> k) ->
  -- | The comments to attach.
  [Comment] ->
  -- | The document to attach them to.
  Doc ->
  Doc
attachScopedComments margin scopeOf cs doc =
  written <> closingComments margin (unplaced left)
  where
    (written, left) = walk margin placements doc
    (regions, fences) = markedSpans doc
    leads = leadingRegions doc
    placements =
      foldMap
        ( \k ->
            placeComments (inScope k rs) leads (inScope k fs) (inScope k xs)
        )
        (Set.toList (Map.keysSet rs <> Map.keysSet fs <> Map.keysSet xs))
    rs = byScope id regions
    fs = byScope id fences
    xs = byScope commentSpan cs
    byScope f ys = reverse <$> Map.fromListWith (<>) [(scopeOf (f y), [y]) | y <- ys]
    inScope = Map.findWithDefault []

-- | The comments nothing came to collect, written after everything.
closingComments ::
  -- | What goes at the margin.
  Margin ->
  -- | The comments.
  [Comment] ->
  Doc
closingComments margin = \case
  [] -> mempty
  (opening : rest) -> placeOne True opening <> foldMap (placeOne False) rest
  where
    placeOne opensTheRun c =
      commentDoc c $
        closeLine
          <> includeWhen (opensTheRun || commentGapAbove c) blankLine
          <> commentText margin c
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
  DCppChoice _ bs e -> foldMap (markedSpans . snd) bs <> markedSpans e
  _ -> ([], [])

-- | The innermost region each region prints first, where that one was
-- written above it, as the Haddock of a constructor, a field or an argument
-- is.
leadingRegions :: Doc -> Map Span Span
leadingRegions = \case
  DLocated s d
    | Just h <- firstRegion d,
      startPoint h < startPoint s ->
        Map.insert s h (leadingRegions d)
  d -> foldChildren leadingRegions d

-- | The innermost region a document prints first.
firstRegion :: Doc -> Maybe Span
firstRegion = getFirst . go
  where
    go = \case
      DLocated s d -> First (firstRegion d <|> Just s)
      d -> foldChildren go d

-- | Write the placed comments into the document, each one around the region
-- it was given to. Taking is destructive: the 'Placements' is threaded
-- through the walk and a region's comments are removed as they are written,
-- so a span that occurs twice is served once. The two sides of a 'DVariant'
-- are the exception—they are one piece of code laid out two ways, so both
-- are walked from the same 'Placements' and both come out holding the
-- comment, and only one of them is ever printed. So are the alternatives of
-- a 'DCppChoice', for the same reason: no configuration prints two of them,
-- and a region the merge wrote into several of them is printed once in each
-- configuration, comments and all. What is still unwritten when the walk
-- ends is returned next to the resulting 'Doc'.
walk ::
  -- | What goes at the margin.
  Margin ->
  -- | Where each comment goes.
  Placements ->
  -- | The document to write them into.
  Doc ->
  (Doc, Placements)
walk margin = go
  where
    go p = \case
      DCat a b ->
        let (a', p') = go p a
            (b', p'') = go p' b
         in (DCat a' b', p'')
      DLocated s d ->
        let (mine, p') = claimPlaced s p
            (d', p'') = go p' d
            write = foldMap (uncurry (writtenAs margin (isEmptyAnchor s)))
            before' =
              (Before,) <$> heldOffFrom d [c | (Before, c) <- mine]
            after' =
              [(q, c) | (q, c) <- mine, q == After || q == UnderTheRemark]
            under' = [(q, c) | (q, c) <- mine, q == Under]
            withUnder
              | null under' = id
              | otherwise = \x -> DAlign (x <> write under')
         in ( write before' <> withUnder (DLocated s d' <> write after'),
              p''
            )
      DFence s d -> first (DFence s) (go p d)
      DCppChoice cs bs e ->
        let bs' = [(c, go p d) | (c, d) <- bs]
            (e', pe) = go p e
         in ( DCppChoice cs [(c, d') | (c, (d', _)) <- bs'] e',
              foldr (unclaimedByEither . snd . snd) pe bs'
            )
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

-- | Does this region begin its first line with a Haddock other than a
-- section heading?
opensWithHaddock :: Doc -> Bool
opensWithHaddock d = case listToMaybe (fst (firstLine Broken d)) of
  Just l -> opensHaddock l && not (opensSectionHeading l)
  Nothing -> False
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
      DCloseLineUnlessAfterOpener _ -> ([], True)
      DBreak -> ([], layout == Broken)
      DSoftBreak -> ([], layout == Broken)
      _ -> ([], False)

-- | Is this region an 'emptyAnchor' rather than one covering something
-- written?
isEmptyAnchor :: Span -> Bool
isEmptyAnchor s = startPoint s == endPoint s

-- | Render one 'Comment' where it was placed.
writtenAs ::
  -- | What goes at the margin.
  Margin ->
  -- | Does what follows only mark where the construct ends?
  Bool ->
  -- | Comment position.
  Position ->
  -- | The comment to render.
  Comment ->
  Doc
writtenAs margin atTheEnd position c = commentDoc c $ case shapeOf position c of
  InPlace -> case position of
    Before -> includeUnless glued space <> body <> includeUnless atTheEnd space
    _ -> space <> body
  EndsTheLine -> space <> body <> closeLine <> gapBelow
  HeldBack -> holdBack spill (renderComment c)
  OnItsOwnLines ->
    closeLineUnlessAfterOpener (commentGapAbove c)
      <> body
      <> closeLine
      <> gapBelow
  where
    glued = commentTrailing c && (not atTheEnd || commentFirstInBrackets c)
    body = commentText margin c
    gapBelow = includeWhen (commentGapBelow c && not atTheEnd) blankLine
    spill
      | position == UnderTheRemark = SpillUnderPrevious
      | otherwise = SpillAtIndentation

-- | A comment, and the spacing that goes with it, as one region.
commentDoc :: Comment -> Doc -> Doc
commentDoc = located . commentSpan

-- | The text of a comment, laid out as it was written.
commentText ::
  -- | What goes at the margin.
  Margin ->
  -- | The comment.
  Comment ->
  Doc
commentText margin c = note . align $ case NE.toList (commentBody c) of
  [] -> mempty
  l : ls -> txt l <> foldMap (\x -> breakBefore x <> txt x) ls
  where
    breakBefore x
      | marginLine margin x = verbatimBreak AtMargin TrimWhitespace
      | otherwise = verbatimBreak AtIndent TrimWhitespace
    note
      | marginComment margin c = cppMarginNote
      | otherwise = id

-- | Put this text at the end of the line this position falls on.
holdBack :: Spill -> Text -> Doc
holdBack = DHoldBack

-- | End the current line and leave a mark so that a line break that follows
-- it immediately (if any) will have no effect.
closeLine :: Doc
closeLine = DCloseLine

-- | 'closeLine', with an empty line under it if told to, unless the line
-- ends with an opening bracket, a bar or an equals sign.
closeLineUnlessAfterOpener :: Bool -> Doc
closeLineUnlessAfterOpener = DCloseLineUnlessAfterOpener
