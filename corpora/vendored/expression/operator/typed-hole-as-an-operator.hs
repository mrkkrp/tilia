module Tally.Draft where

combined :: Int
combined = 3 `_` 4

scaled :: Int
scaled = 3 `_factor` 4 + 1

missing :: Int -> Int
missing n = _ `max` n

pending :: [Int]
pending = [_ | let _ = combined]

later :: Int
later = _
