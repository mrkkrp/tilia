-- | Determine the placement of each 'Comment'.
module Tilia.Comments.Place
  ( Position (..),
    Alignment (..),
    Shape (..),
    shapeOf,
    Placements,
    placeComments,
    claimPlaced,
    unclaimedByEither,
    unplaced,
    nothingPlaced,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Tilia.Comments
  ( Above (..),
    Comment (..),
    carriedOnFrom,
    closesItself,
    commentTrailing,
    singleLine,
  )
import Tilia.Doc.Internal (Spill (..))
import Tilia.Span

-- | Where a comment is emitted in relation to its region.
data Position
  = -- | Before the region.
    Before
  | -- | After the region, on the line it ends on.
    After Spill
  | -- | On lines of its own under the region.
    Under Alignment
  deriving (Eq, Show)

-- | What a comment on lines of its own under a region is lined up with.
data Alignment
  = -- | The column the region begins at.
    ByTheRegion
  | -- | The indentation in force after the region.
    ByTheIndentation
  deriving (Eq, Show)

-- | How a comment is printed in relation to the region carrying it.
data Shape
  = -- | Spliced where the region prints, with code able to follow it on the
    -- same line.
    InPlace
  | -- | Printed where the region is, and the line closed after it.
    EndsTheLine
  | -- | Held back to the end of whatever line of output it lands on,
    -- however much of that line is still to be written.
    HeldBack Spill
  | -- | On lines of its own, keeping the empty lines the author left around
    -- it.
    OnItsOwnLines
  deriving (Eq, Show)

-- | What a comment given to a region at this position will look like.
shapeOf :: Position -> Comment -> Shape
shapeOf position c = case position of
  Before
    | closesItself c && commentFollowed c -> InPlace
    | commentTrailing c -> EndsTheLine
    | otherwise -> OnItsOwnLines
  After spill
    | closesItself c -> InPlace
    | singleLine c -> HeldBack spill
    | otherwise -> EndsTheLine
  Under _ -> OnItsOwnLines

-- | Comment placements not yet written: all of them when placement is
-- decided, fewer as a walk writes them.
data Placements = Placements
  { -- | The comments to write around each region.
    placedAt :: Map Span [(Position, Comment)],
    -- | The comments no region was found for.
    placedNowhere :: [Comment]
  }

instance Semigroup Placements where
  a <> b =
    Placements
      { placedAt = Map.unionWith (<>) (placedAt a) (placedAt b),
        placedNowhere = placedNowhere a <> placedNowhere b
      }

instance Monoid Placements where
  mempty = Placements Map.empty []

-- | Determine comment placements.
placeComments ::
  -- | The regions a comment may be given to.
  [Span] ->
  -- | The innermost region each region prints first, where that one was
  -- written above it.
  Map Span Span ->
  -- | The boundaries a comment printed in place may not be carried across.
  [Span] ->
  -- | The comments to place.
  [Comment] ->
  Placements
placeComments regions leads fences comments =
  Placements
    { placedAt = Map.fromListWith (flip (<>)) [(r, [(p, c)]) | (Just (r, p), c) <- decided],
      placedNowhere = [c | (Nothing, c) <- decided]
    }
  where
    decided = [(against c, c) | c <- comments]

    carriedOn = carriedOnFrom comments

    regionsByEndLine =
      IntMap.fromListWith (<>) [(spanEndLine r, [r]) | r <- regions]

    regionEndPoints = Set.fromList (fmap endPoint regions)

    regionsByStartPoint =
      Map.fromListWith wider [(startPoint r, r) | r <- regions]
      where
        wider a b = if endPoint a >= endPoint b then a else b

    against c
      | Just placed@(_, After _) <- asCommentBefore = Just placed
      | commentTrailing c,
        Just r <- trailed =
          Just (r, After SpillAtIndentation)
      | Just placed <- continues = Just placed
      | Just placed <- under = Just placed
      | Just r <- next =
          Just (maybe (r, Before) (,Under ByTheRegion) (Map.lookup r leads))
      | otherwise = Nothing
      where
        here = commentSpan c
        asCommentBefore = against =<< (`Map.lookup` commentsByEndPoint) =<< stopsAt
        trailed
          | writtenAgainst || not (commentFollowed c) =
              linedUpUnder <|> endingOn (spanStartLine here)
          | otherwise = Nothing
        linedUpUnder = do
          (top, _) <- IntMap.lookup (spanStartLine here + 1) runs
          (r, Under ByTheRegion) <- against top
          r <$ guard (candidate r)
        endingOn line =
          nearest (\r -> (Down (endPoint r), startPoint r)) (filter candidate onThatLine)
          where
            onThatLine = IntMap.findWithDefault [] line regionsByEndLine
        candidate r =
          endPoint r <= startPoint here
            && startPoint r /= endPoint r
            && not (fencedOff r)

        writtenAgainst = maybe False (`Set.member` regionEndPoints) stopsAt
        stopsAt = (,) (spanStartLine here) <$> commentCodeBeforeStopsAt c

        fencedOff r =
          outside (Map.lookup here enclosingRegions)
            || (printedInPlace && outside (Map.lookup here enclosingFences))
          where
            outside = maybe False (\(from, to) -> not (from <= startPoint r && endPoint r <= to))

        printedInPlace = shapeOf (After SpillAtIndentation) c == InPlace

        next = snd <$> Map.lookupGE (endPoint here) regionsByStartPoint

        continues
          | nothingBelowItLinesUp,
            Just (anchor, remark) <- carriedOn c =
              (,linedUpWith remark) <$> endingOn anchor
          | otherwise = Nothing

        linedUpWith remark
          | remark == spanStartColumn here = After SpillUnderPrevious
          | otherwise = After SpillAtIndentation

        nothingBelowItLinesUp =
          all (\r -> spanStartColumn r < spanStartColumn here) next

        under
          | Just (top, bottom) <- IntMap.lookup (spanStartLine here) runs,
            Just (line, column) <- commentNextLine bottom,
            column /= spanStartColumn here,
            any ((<= (line, column)) . startPoint) next =
              lineOfCode top
          | otherwise = Nothing

        lineOfCode top = do
          r <- nearest (Down . startPoint) (filter linedUp onThatLine)
          guard (any (larger r) onThatLine)
          pure (r, Under (alignedBy r))
          where
            line = spanStartLine (commentSpan top) - 1
            onThatLine = IntMap.findWithDefault [] line regionsByEndLine
            lineEnd = maximum (fmap endPoint onThatLine)
            linedUp r =
              startPoint r /= endPoint r
                && endPoint r == lineEnd
                && (startsUnderIt r || holdsTheLine r)
                && not (fencedOff r)
            startsUnderIt r =
              spanStartColumn r == spanStartColumn here
                && (spanStartLine r == line || startsItsLine r)
            holdsTheLine r =
              commentAbove top == ContentAt (spanStartColumn here)
                && startPoint r <= (line, spanStartColumn here)
            alignedBy r
              | startsUnderIt r = ByTheRegion
              | otherwise = ByTheIndentation
            startsItsLine r =
              maybe True ((< spanStartLine r) . fst . fst) $
                Map.lookupLT (startPoint r) regionsByStartPoint
            larger r o = startPoint o < startPoint r && endPoint o == lineEnd

    commentsByEndPoint = Map.fromList [(endPoint (commentSpan c), c) | c <- comments]

    runs =
      IntMap.fromList
        [ (spanStartLine (commentSpan c), (NE.head run, NE.last run))
        | run <- foldr joined [] (sortOn (startPoint . commentSpan) comments),
          c <- NE.toList run
        ]
      where
        joined c rest
          | commentTrailing c || commentFollowed c = rest
          | (d :| ds) : rest' <- rest, continuedBy c d = (c :| d : ds) : rest'
          | otherwise = (c :| []) : rest
        continuedBy c d =
          spanStartLine below == spanEndLine above + 1
            && spanStartColumn below == spanStartColumn above
          where
            above = commentSpan c
            below = commentSpan d

    enclosingRegions = enclosures regions (fmap commentSpan comments)
    enclosingFences = enclosures fences (fmap commentSpan comments)

    nearest :: (Ord k) => (Span -> k) -> [Span] -> Maybe Span
    nearest key = fmap fst . foldl' closer Nothing
      where
        closer best s = case best of
          Just (_, k) | k <= key s -> best
          _ -> Just (s, key s)

-- | For each inner span that outer spans enclose, the latest start and the
-- earliest end among them.
enclosures :: [Span] -> [Span] -> Map Span ((Int, Int), (Int, Int))
enclosures outer inner =
  Map.fromList (go (sortOn startPoint outer) Map.empty Set.empty (sortOn startPoint inner))
  where
    go _ _ _ [] = []
    go os latest ends (i : is) =
      let (opened, later) = span ((<= startPoint i) . startPoint) os
          latest' = foldl' admit latest opened
          ends' = foldl' (flip (Set.insert . endPoint)) ends opened
          bounds = do
            (_, from) <- Map.lookupGE (endPoint i) latest'
            to <- Set.lookupGE (endPoint i) ends'
            pure (i, (from, to))
       in maybe id (:) bounds (go later latest' ends' is)
    -- A span opened later starts no earlier, so it supersedes those that end
    -- no later than it does.
    admit latest o =
      Map.insert (endPoint o) (startPoint o) (Map.dropWhileAntitone (<= endPoint o) latest)

-- | Claim the comments that belong to the given 'Span'.
claimPlaced :: Span -> Placements -> ([(Position, Comment)], Placements)
claimPlaced s p = case Map.updateLookupWithKey forget s (placedAt p) of
  (found, rest) -> (concat found, p{placedAt = rest})
  where
    forget _ _ = Nothing

-- | The placements neither of two walks from the same placements claimed.
unclaimedByEither ::
  Placements ->
  Placements ->
  Placements
unclaimedByEither a b =
  a{placedAt = Map.intersection (placedAt a) (placedAt b)}

-- | The comments no region ever came to collect.
unplaced :: Placements -> [Comment]
unplaced p =
  sortOn (startPoint . commentSpan) $
    placedNowhere p <> foldMap (fmap snd) (placedAt p)

-- | Is there nothing left for a region to collect?
nothingPlaced :: Placements -> Bool
nothingPlaced = Map.null . placedAt
