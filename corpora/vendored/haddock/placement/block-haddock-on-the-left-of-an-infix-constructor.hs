module Kiln.Infix where

-- A block Haddock ends where it closes, so the constructor stays on the
-- line it is written on.
data Ratio = Integer {- ^ the numerator -} :% Integer

data Interval
  = Double {- ^ the lower bound -}
      :.. Double
