module Ledger.Total where

total :: ([Int], Int) -> Int
total ((items, carried)) = sum items + carried

single :: [Int] -> Maybe Int
single (([x])) = Just x
single _ = Nothing
