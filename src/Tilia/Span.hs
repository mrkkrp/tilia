-- | Regions of the input, and the questions asked about them.
module Tilia.Span
  ( Span (..),
    mkSpan,
    isSingleLine,
    sameLine,
    blankBetween,
    meets,
    covers,
    startOf,
    endOf,
    startPoint,
    endPoint,
  )
where

-- | A region of the input.
data Span = Span
  { spanStartLine :: !Int,
    spanStartColumn :: !Int,
    spanEndLine :: !Int,
    spanEndColumn :: !Int
  }
  deriving (Eq, Ord, Show)

-- | Build a 'Span' from start and end positions, each a line and a column.
mkSpan :: (Int, Int) -> (Int, Int) -> Span
mkSpan (sl, sc) (el, ec) = Span sl sc el ec

-- | The smallest span covering both arguments.
instance Semigroup Span where
  a <> b =
    Span
      { spanStartLine = min (spanStartLine a) (spanStartLine b),
        spanStartColumn = case compare (spanStartLine a) (spanStartLine b) of
          LT -> spanStartColumn a
          GT -> spanStartColumn b
          EQ -> min (spanStartColumn a) (spanStartColumn b),
        spanEndLine = max (spanEndLine a) (spanEndLine b),
        spanEndColumn = case compare (spanEndLine a) (spanEndLine b) of
          GT -> spanEndColumn a
          LT -> spanEndColumn b
          EQ -> max (spanEndColumn a) (spanEndColumn b)
      }

-- | Did this occupy a single line of the input?
isSingleLine :: Span -> Bool
isSingleLine s = spanStartLine s == spanEndLine s

-- | Did the second thing begin on the line the first thing ended on?
sameLine :: Maybe Span -> Maybe Span -> Bool
sameLine (Just a) (Just b) = spanEndLine a == spanStartLine b
sameLine _ _ = False

-- | Was there an empty line between the two?
blankBetween :: Maybe Span -> Maybe Span -> Bool
blankBetween (Just a) (Just b) = spanStartLine b > spanEndLine a + 1
blankBetween _ _ = False

-- | Do the two cover any of the same input?
--
-- Touching counts: a span ending where the next begins shares that
-- position, and the callers that ask this are asking whether the two are
-- looking at one thing, not whether either strictly contains the other.
meets :: Span -> Span -> Bool
meets a b = startPoint a <= endPoint b && startPoint b <= endPoint a

-- | Does the first cover all of the second?
covers :: Span -> Span -> Bool
covers a b = startPoint a <= startPoint b && endPoint b <= endPoint a

-- | A zero-width span at the start of the given one.
startOf :: Span -> Span
startOf s = at (spanStartLine s, spanStartColumn s)

-- | A zero-width span at the end of the given one.
endOf :: Span -> Span
endOf s = at (spanEndLine s, spanEndColumn s)

-- | A zero-width span at the given position.
at :: (Int, Int) -> Span
at position = mkSpan position position

-- | Where a span begins, as a position two of them may be compared by.
startPoint :: Span -> (Int, Int)
startPoint s = (spanStartLine s, spanStartColumn s)

-- | Where a span ends.
endPoint :: Span -> (Int, Int)
endPoint s = (spanEndLine s, spanEndColumn s)
