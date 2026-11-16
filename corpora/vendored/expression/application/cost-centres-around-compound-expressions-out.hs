module Profile.Report where

summary :: [Int] -> Int
summary xs = {-# SCC "summary" #-} (sum xs + length xs)

counted :: [Int] -> [Int]
counted xs = {-# SCC counted #-} [x | x <- xs, x > 0]
