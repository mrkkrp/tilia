-- A comment written after the left argument of an infix constructor stays
-- out of the argument's Haddock, which goes on the line under it.
module Geometry.Walk where

data Step
    = Int -- in metres
      -- ^ A step to the left.
        :<=
        -- | how many steps
        Int

data Stride
    = Int -- \^ how far
      -- ^ A stride to the left.
        :<<
        Int
