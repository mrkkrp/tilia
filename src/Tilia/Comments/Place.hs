-- | Determine the placement of each 'Comment'.
module Tilia.Comments.Place
  ( Position (..),
    Shape (..),
    shapeOf,
    Placements,
    placeComments,
    claimPlaced,
    unplaced,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Tilia.Comments
  ( Above (..),
    Comment (..),
    closesItself,
    commentTrailing,
    singleLine,
  )
import Tilia.Span

-- | Which side of its region a comment is emitted on.
data Position
  = -- | Before the region.
    Before
  | -- | After the region.
    After
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
    HeldBack
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
  After
    | closesItself c -> InPlace
    | singleLine c -> HeldBack
    | otherwise -> EndsTheLine

-- | Comment placements.
data Placements = Placements
  { placedAt :: Map Span [(Position, Comment)],
    placedNowhere :: [Comment]
  }

-- | Determine comment placements.
placeComments ::
  -- | The regions a comment may be given to.
  [Span] ->
  -- | The boundaries a comment printed in place may not be carried across.
  [Span] ->
  -- | The comments to place.
  [Comment] ->
  Placements
placeComments regions fences comments =
  Placements
    { placedAt = Map.fromListWith (flip (<>)) [(r, [(p, c)]) | (Just (r, p), c) <- decided],
      placedNowhere = [c | (Nothing, c) <- decided]
    }
  where
    decided = [(against c, c) | c <- comments]

    linesEndingInAComment =
      IntSet.fromList
        [ spanEndLine (commentSpan c)
        | c <- comments,
          commentTrailing c,
          not (commentFollowed c)
        ]

    ownLineComments =
      IntMap.fromList
        [ (spanStartLine s, (spanStartColumn s, commentAbove c))
        | c <- comments,
          not (commentTrailing c),
          not (commentFollowed c),
          let s = commentSpan c
        ]

    carriedOnFrom column = go
      where
        go line
          | IntSet.member line linesEndingInAComment = Just line
          | Just (col, above) <- IntMap.lookup line ownLineComments,
            col == column,
            above == ContentAt column =
              go (line - 1)
          | otherwise = Nothing

    regionsByEndLine =
      IntMap.fromListWith (<>) [(spanEndLine r, [r]) | r <- regions]

    regionEndPoints = Set.fromList (map endPoint regions)

    regionsByStartPoint =
      Map.fromListWith wider [(startPoint r, r) | r <- regions]
      where
        wider a b = if endPoint a >= endPoint b then a else b

    against c
      | commentTrailing c, Just r <- trailed = Just (r, After)
      | not (commentTrailing c), Just r <- continues = Just (r, After)
      | Just r <- next = Just (r, Before)
      | otherwise = Nothing
      where
        here = commentSpan c
        trailed
          | writtenAgainst || not (commentFollowed c) = endingOn (spanStartLine here)
          | otherwise = Nothing
        endingOn line =
          nearest (\r -> (Down (endPoint r), startPoint r)) (filter candidate onThatLine)
          where
            onThatLine = IntMap.findWithDefault [] line regionsByEndLine
            candidate r = endPoint r <= startPoint here && not (fencedOff r)

        writtenAgainst = maybe False (`Set.member` regionEndPoints) stopsAt
        stopsAt = (,) (spanStartLine here) <$> commentCodeBeforeStopsAt c

        enclosingRegions = filter (here `inside`) regions
        enclosingFences = filter (here `inside`) fences

        fencedOff r = outside enclosingRegions || (printedInPlace && outside enclosingFences)
          where
            outside = any (not . (r `inside`))

        printedInPlace = shapeOf After c == InPlace

        next = snd <$> Map.lookupGE (endPoint here) regionsByStartPoint

        continues
          | ContentAt column <- commentAbove c,
            column == spanStartColumn here,
            nothingBelowItLinesUp,
            Just anchor <- carriedOnFrom column (spanStartLine here - 1) =
              endingOn anchor
          | otherwise = Nothing

        nothingBelowItLinesUp =
          all (\r -> spanStartColumn r < spanStartColumn here) next

    nearest :: (Ord k) => (Span -> k) -> [Span] -> Maybe Span
    nearest key = fmap fst . foldl' closer Nothing
      where
        closer best s = case best of
          Just (_, k) | k <= key s -> best
          _ -> Just (s, key s)

-- | Does the first region fall within the second?
inside :: Span -> Span -> Bool
inside a b = startPoint b <= startPoint a && endPoint a <= endPoint b

-- | Claim the comments that belong to the given 'Span'.
claimPlaced :: Span -> Placements -> ([(Position, Comment)], Placements)
claimPlaced s p = case Map.updateLookupWithKey forget s (placedAt p) of
  (found, rest) -> (concat found, p {placedAt = rest})
  where
    forget _ _ = Nothing

-- | The comments no region ever came to collect.
unplaced :: Placements -> [Comment]
unplaced p =
  sortOn (startPoint . commentSpan) $
    placedNowhere p <> foldMap (fmap snd) (placedAt p)
