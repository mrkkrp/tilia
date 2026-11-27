{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Attaching comments to a 'Doc'.
module Tilia.Comments.Attach
  ( attachComments,
    attachScopedComments,
    Margin (..),
    commentText,
    joinedLine,
  )
where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.List (unsnoc)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Monoid (Endo (..), First (..), Last (..))
import Data.Semigroup (Max (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

import Tilia.Comments
import Tilia.Comments.Place
import Tilia.Doc
  ( BracketSide (..),
    Doc (..),
    SeparatorSide (..),
    chosen,
    foldChildren,
    layoutInside,
    namesOnly,
    printsNothing,
    unwrap,
    wrap,
    pattern Cat,
  )
import Tilia.Doc.Combinators
import Tilia.Span

-- | Attach comments to a 'Doc'.
attachComments :: [Comment] -> Doc -> Doc
attachComments = attachScopedComments noMargin (const ())

-- | What parts of a comment go at the margin.
data Margin = Margin
  { -- | A line of the module, by its number, that a comment written over it
    -- prints at the margin, as a preprocessor directive written in one has
    -- to: the preprocessor reads directives out of comments as well.
    marginLine :: Int -> Bool,
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
  Ord k =>
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
    (regions, fences, openings) = markedSpans doc
    gaps = gappedItems doc
    leads = leadingRegions doc
    elsewhere = endingElsewhere doc
    placements =
      foldMap
        ( \k ->
            placeComments
              (inScope k rs)
              leads
              elsewhere
              openings
              gaps
              (inScope k fs)
              (inScope k xs)
        )
        (Set.toList (Map.keysSet rs <> Map.keysSet fs <> Map.keysSet xs))
    rs = byScope id regions
    fs = byScope id fences
    xs = byScope commentSpan cs
    byScope f ys =
      reverse <$> Map.fromListWith (<>) [(scopeOf (f y), [y]) | y <- ys]
    inScope = Map.findWithDefault []

-- | The comments nothing came to collect, written after everything.
closingComments ::
  -- | What goes at the margin.
  Margin ->
  -- | The comments.
  [Comment] ->
  Doc
closingComments margin = foldMap $ \c -> commentDoc c (ownLines margin True c)

-- | The spans of every 'DLocated' in the document, of every 'DFence', and
-- of every opening bracket, in that order.
markedSpans :: Doc -> ([Span], [Span], [Span])
markedSpans d = go d ([], [], [])
  where
    go = \case
      DLocated s x -> (\ ~(rs, fs, os) -> (s : rs, fs, os)) . go x
      DFence s x -> (\ ~(rs, fs, os) -> (rs, s : fs, os)) . go x
      DBracket OpeningBracket (Just s) x ->
        (\ ~(rs, fs, os) -> (rs, fs, s : os)) . go x
      x -> appEndo (foldChildren (Endo . go) x)

-- | The region that finishes printing last before each 'DGap', where no
-- directive comes after it: the item of a run that the empty line is under.
gappedItems :: Doc -> [Span]
gappedItems d = snd (go d (Nothing, []))
  where
    go x (above, found) = case x of
      DLocated _ y@DCppChoice{} -> go y (above, found)
      DLocated s y
        | zeroWidth s -> go y (above, found)
        | otherwise -> (Just s, snd (go y (above, found)))
      DCppChoice _ bs e ->
        ( Nothing,
          foldl (\f a -> snd (go a (Nothing, f))) found (fmap snd bs <> [e])
        )
      DCppDirective{} -> (Nothing, found)
      DGap y -> go y (above, maybe id (:) above found)
      _ -> foldl (flip go) (above, found) (foldChildren (: []) x)

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

-- | The regions that do not print last what was written at their end,
-- where they print anything written that far, as a signature prints the
-- Haddock of its last argument under that argument.
endingElsewhere :: Doc -> Set Span
endingElsewhere = (\(_, _, found) -> found) . go
  where
    go = \case
      DLocated s d -> spanning s d (Set.insert s)
      DFence s d -> spanning s d id
      d -> foldChildren go d
    spanning s d flag =
      let (lastOne, furthest, found) = go d
          elsewhere = case (getLast lastOne, furthest) of
            (Just (l, further), Just (Max f)) ->
              further || f >= endPoint s && endPoint l /= endPoint s
            _ -> False
       in ( Last (Just (s, elsewhere)),
            Just (Max (maybe id (max . getMax) furthest (endPoint s))),
            if elsewhere then flag found else found
          )

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
    go p d
      | nothingPlaced p = (d, p)
      | otherwise = step p d
    step p = \case
      DIndented n a b | not (namesOnly a) -> both (DIndented n) p a b
      Cat a b -> both DCat p a b
      DLocated s d -> case claimPlaced s p of
        ([], p') -> first (DLocated s) (go p' d)
        (mine, p') ->
          let (d', p'') = go p' d
              write = foldMap (uncurry (writtenAs margin (zeroWidth s)))
              before' =
                (Before,) <$> heldOffFrom d [c | (Before, c) <- mine]
              after' =
                [(q, c) | (q, c) <- mine, q /= Before, not (linedUpWithIt q)]
              under' = [(q, c) | (q, c) <- mine, linedUpWithIt q]
              linedUpWithIt = \case
                Under ByTheRegion -> True
                Under ByTheIndentation -> alignsItself d
                _ -> False
              withUnder
                | null under' = id
                | otherwise = \x -> align (x <> write under')
           in ( write before' <> withUnder (DLocated s d' <> write after'),
                p''
              )
      DCppChoice cs bs e ->
        let bs' = [(c, go p d) | (c, d) <- bs]
            (e', pe) = go p e
         in ( DCppChoice cs [(c, d') | (c, (d', _)) <- bs'] e',
              foldr (unclaimedByEither . snd . snd) pe bs'
            )
      DVariant by a b ->
        let (a', _) = go p a
            (b', p') = go p b
         in (DVariant by a' b', p')
      d
        | Just (w, x) <- unwrap d -> first (wrap w) (go p x)
        | otherwise -> (d, p)
    both joined p a b
      | leadsAnItem a,
        Just s <- openingRegion b,
        (lifted@(_ : _), p0) <- claimOwnLines s p =
          let (a', p') = go p0 a
              (b', p'') = go p' b
           in ( foldMap (\c -> commentDoc c (ownLines margin False c)) lifted
                  <> joined a' b',
                p''
              )
      | otherwise =
          let (a', p') = go p a
              (b', p'') = go p' b
           in (joined a' b', p'')

-- | Does a document line its lines up under the column it begins at?
alignsItself ::
  -- | The document.
  Doc ->
  Bool
alignsItself = \case
  DAligned 0 _ -> True
  DLocated _ d -> alignsItself d
  DFence _ d -> alignsItself d
  DGroup _ d -> alignsItself d
  _ -> False

-- | Is this a separator hung left of the item it leads, alone or after the
-- place the line may break before it?
leadsAnItem ::
  -- | The document.
  Doc ->
  Bool
leadsAnItem = \case
  DCat DSoftBreak d -> hung d
  d -> hung d
  where
    hung = \case
      DSeparator LeadingSeparator (DAligned n _) _ -> n < 0
      _ -> False

-- | The outermost region a document prints first.
openingRegion ::
  -- | The document.
  Doc ->
  Maybe Span
openingRegion = \case
  DLocated s _ -> Just s
  DCat a b -> after a b
  DIndented _ a b | not (namesOnly a) -> after a b
  DVariant by a b -> openingRegion (chosen by Broken a b)
  d -> openingRegion . snd =<< unwrap d
  where
    after a b
      | printsNothing a = openingRegion a <|> openingRegion b
      | otherwise = openingRegion a

-- | Hold the last comment, if written as @--@ lines, off a Haddock about to
-- be written under it.
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
      DCat a b -> both layout a b
      DIndented _ a b | not (namesOnly a) -> both layout a b
      DVariant by a b -> firstLine layout (chosen by layout a b)
      DHardBreak -> ([], True)
      DCloseLine -> ([], True)
      DBreak -> ([], layout == Broken)
      DSoftBreak -> ([], layout == Broken)
      y
        | Just (w, x) <- unwrap y -> firstLine (layoutInside layout w) x
        | otherwise -> ([], False)
    both layout a b = case firstLine layout a of
      (before, True) -> (before, True)
      (before, False) -> first (before <>) (firstLine layout b)

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
    Before ->
      includeUnless glued space
        <> DComment InPlace (commentSpan c) body
        <> includeUnless atTheEnd space
    _ -> space <> DComment InPlace (commentSpan c) body
  EndsTheLine -> DComment EndsTheLine (commentSpan c) body <> gapBelow
  shape@(HeldBack _) ->
    DComment
      shape
      (commentSpan c)
      (body <> includeWhen gapLeft (DVerbatimBreak AtIndent TrimWhitespace))
  OnItsOwnLines _ -> ownLines margin atTheEnd c
  where
    glued = commentTrailing c && (not atTheEnd || commentFirstInBrackets c)
    body = commentText margin c
    gapBelow = includeWhen (commentGapBelow c && not atTheEnd) blankLine
    gapLeft =
      not (commentTrailing c)
        && not (commentJoined c)
        && commentGapBelow c
        && all ((> spanEndLine (commentSpan c) + 1) . fst) (commentNextLine c)

-- | A comment on lines of its own, with the empty lines its author left
-- around it.
ownLines ::
  -- | What goes at the margin.
  Margin ->
  -- | Does what follows only mark where the construct ends?
  Bool ->
  -- | The comment.
  Comment ->
  Doc
ownLines margin atTheEnd c =
  DComment
    (OnItsOwnLines (commentGapAbove c))
    (commentSpan c)
    (commentText margin c)
    <> includeWhen (commentGapBelow c && not atTheEnd) blankLine

-- | A comment, and the spacing that goes with it, as one region.
commentDoc :: Comment -> Doc -> Doc
commentDoc = located . commentSpan

-- | The text of a comment, laid out as it was written, and the empty line
-- cpp joins to it.
commentText ::
  -- | What goes at the margin.
  Margin ->
  -- | The comment.
  Comment ->
  Doc
commentText margin c = note . align $ case numbered c of
  [] -> mempty
  (_, l) : ls ->
    txt l
      <> foldMap (\(n, x) -> breakBefore n <> txt x) ls
      <> joinedLine c
  where
    breakBefore n
      | marginLine margin n = DVerbatimBreak AtMargin TrimWhitespace
      | otherwise = DVerbatimBreak AtIndent TrimWhitespace
    note
      | marginComment margin c = DCppMarginNote
      | otherwise = id

-- | The empty line cpp joins to a comment, if it joins one, reproduced
-- under it.
joinedLine ::
  -- | The comment.
  Comment ->
  Doc
joinedLine c =
  includeWhen (commentJoined c) (DVerbatimBreak AtMargin TrimWhitespace)

-- | The lines of a comment, each with the number of the line of the module
-- it was written on.
numbered ::
  -- | The comment.
  Comment ->
  [(Int, Text)]
numbered c = zip [spanStartLine (commentSpan c) ..] (NE.toList (commentBody c))
