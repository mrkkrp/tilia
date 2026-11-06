module Shapes.Perimeter where

import Shapes.Kind
  ( Shape
      ( Circle,
        Square
      ),
    Side
      ( Bottom,
        Top
      ),
    perimeter,
  )

total :: [Shape] -> Double
total = sum . fmap perimeter
