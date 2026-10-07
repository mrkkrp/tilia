module Tariff.Discount where

discount :: Int -> Bool -> Int
discount age student
  | -- children travel free
    age < 12 = 0
  | {- students -} student = 50
  | otherwise = 100
