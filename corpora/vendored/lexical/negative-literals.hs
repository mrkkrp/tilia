module Thermostat.Offset where

lower :: Int -> Int
lower x = subtract (-1) x

below :: Double
below = negate 2 * (-0.5)

opposite :: Int -> Int
opposite n = - n
