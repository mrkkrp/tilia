module Shapes.Perimeter where

import Shapes.Kind
  ( Shape
      ( Circle,
        Square
      ),
    Side (Top,
      Bottom),
    perimeter
  )

total :: [Shape] -> Double
total = sum . fmap perimeter
