{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Codec.Digit where

split :: Int -> (# Int, Int #)
split n = (# quot n 10, rem n 10 #)

classify :: (# Int | Char #) -> String
classify _ = "?"

ignored :: (# #) -> Int
ignored (# #) = 0

nothing :: (# #)
nothing = (# #)

spread :: Int -> (#Int,Int,
  Int #)
spread n = (# n, n, n #)
