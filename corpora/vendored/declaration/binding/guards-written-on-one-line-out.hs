module Shipping.Rate where

zone :: Int -> Int
zone km | km < 50 = 1 | km < 400 = 2 | otherwise = 3

expressRate :: Int
expressRate | base <- 12, base > 10 = base * 2 | otherwise = 20
