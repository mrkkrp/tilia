-- stacks of disks, the smallest on top,
-- and never a larger disk on a smaller one
module Hanoi (moves) where

moves :: Int -> a -> a -> a -> [(a, a)]
moves 0 _ _ _ = []
moves n from to via =
  moves (n - 1) from via to ++ [(from, to)] ++ moves (n - 1) via to from
