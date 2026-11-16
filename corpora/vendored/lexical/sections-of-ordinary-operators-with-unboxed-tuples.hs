{-# LANGUAGE UnboxedTuples #-}

-- With unboxed tuples on, only an operator that begins with # needs a space
-- after the opening parenthesis of its section.
module Price.Round where

bumped :: [Int] -> [Int]
bumped = map (+ 1) . map (`div` 2) . map (* 3)
