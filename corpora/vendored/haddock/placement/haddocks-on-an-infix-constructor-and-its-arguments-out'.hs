module Geometry.Span where

data Step
    = Int -- ^ A step to the right.
        :=>
        -- | how far
        Int

data Span
    = Int -- ^ the start
        :->
        -- | the end
        Int
