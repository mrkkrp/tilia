module Shapes.Kind
  ( Shape
      (Circle, Square),
    Side
      ( Top,
        Bottom
      ),
    area
  ) where

data Shape = Circle Double | Square Double

data Side = Top | Bottom

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Square a) = a * a
