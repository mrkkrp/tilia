{-# LANGUAGE ParallelListComp #-}

module Census.Pairs where

paired :: [Int] -> [Int] -> [(Int, Int)]
paired xs ys = [(x, y) | x <- xs, even x | y <- ys, y > 0]

labelled :: [String] -> [Int] -> [String] -> [(String, Int, String)]
labelled as bs cs = [(a, b, c) | a <- as, b <- bs, b `mod` 2 == 0 | c <- cs]
