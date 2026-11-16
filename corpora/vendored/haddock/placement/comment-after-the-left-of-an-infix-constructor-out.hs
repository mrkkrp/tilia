-- A comment written after the left argument of an infix constructor stays
-- out of the argument's Haddock, which is then a block comment.
module Geometry.Walk where

data Step
  = Int {- ^ A step to the left. -} -- in metres
      :<=
      -- | how many steps
      Int

data Stride
  = Int {- ^ A stride to the left. -} -- \^ how far
      :<<
      Int
