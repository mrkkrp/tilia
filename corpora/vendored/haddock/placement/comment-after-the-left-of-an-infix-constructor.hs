-- A comment written after the left argument of an infix constructor stays
-- out of the argument's Haddock, which goes on the line under it.
module Geometry.Walk where

data Step
  = -- | A step to the left.
    Int -- in metres
    :<= Int -- ^ how many steps

data Stride
  = -- | A stride to the left.
    Int -- ^ how far
    :<< Int
