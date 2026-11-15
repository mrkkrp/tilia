{-# LANGUAGE NPlusKPatterns #-}

module Peano.Count where

countdown :: Int -> [Int]
countdown 0 = []
countdown (n+1) = n : countdown n

pairs :: Int -> Int
pairs (n  +  2) = 1 + pairs n
pairs _ = 0
