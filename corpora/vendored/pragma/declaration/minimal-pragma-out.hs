module Geometry.Shape where

class Shape a where
  area :: a -> Double
  perimeter :: a -> Double
  sides :: a -> Int
  {-# MINIMAL area | (perimeter, sides) #-}

class Sized a where
  size :: a -> Int
  {-# MINIMAL size #-}
