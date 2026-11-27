{-# LANGUAGE MultiWayIf #-}

module Tariff.Discount where

discount :: Int -> Bool -> Int
discount age student
    | -- children travel free
      age < 12 = 0
    | otherwise = 100

surcharge :: Int -> Int
surcharge hour =
    if
        | -- the night rate
          hour < 6 -> 20
        | otherwise -> 0
