module Tariff.Zone where

zoneOf :: Double -> Int
zoneOf distance = case distance of
  d
    | d > 10 -> 3
    | d > 5 -> 2
  -- short trips are all zone 1
  _ -> 1

fare :: Int -> Int
fare zone
  | zone > 2 = 4
  | otherwise =
      let base = 2
       in base + zone
  -- the base fare has not changed since the last review

surcharge :: Int
surcharge = 1
