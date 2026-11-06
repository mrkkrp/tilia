{-# LANGUAGE CPP #-}

module Report.Summary where

-- | How many examples ran, and how many of them failed.
data Summary = Summary Int Int

-- Combining summaries.
instance Monoid Summary where
  mempty = Summary 0 0

#if MIN_VERSION_base(4,11,0)
instance Semigroup Summary where
#endif
  Summary x1 x2 `mappend` Summary y1 y2 = Summary (x1 + y1) (x2 + y2)

isSuccess :: Summary -> Bool
isSuccess (Summary _ failures) = failures == 0
