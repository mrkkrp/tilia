module Geometry.Span where

data Step
  = -- | A step to the right.
    Int :=> Int -- ^ how far

data Span
  = Int -- ^ the start
    :-> Int -- ^ the end
