-- A lambda keeps a space before a lazy or a strict first pattern, without
-- which the two would read as one operator.
module Stream.Pairs where

firstOf :: (a, b) -> a
firstOf ~(a, _) = a

swapLazily :: (a, b) -> (b, a)
swapLazily = \ ~(a, b) -> (b, a)

count :: [a] -> Int
count = foldl (\ !n _ -> n + 1) 0
