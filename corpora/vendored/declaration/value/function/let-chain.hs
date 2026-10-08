module Kiln.Glaze where

-- A let ending its line with in, and followed by another let, keeps that
-- line, and the next let goes under it; the last one is laid out as usual.
glaze :: Int -> (Int, Int)
glaze x =
  -- Count the first one:
  let a = x + 1 in
  let b = a * 2; c = b + 1 in
  -- Then the second:
  let d = c - 1 in
  (a, d)

-- Written all on one line, the chain stays on it.
fire :: Int -> Int
fire x = let a = x + 1 in let b = a * 2 in a + b
