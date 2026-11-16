module Calendar.Slots where

afterLunch :: Int -> [Int]
afterLunch lunch = [lunch + 1 ..]

everyOtherFrom :: Int -> [Int]
everyOtherFrom start = [start, start + 2 ..]

working :: Int -> Int -> [Int]
working open close = [open + 1 .. close - 1]

countdown :: Int -> [Int]
countdown n = [n * 2, n * 2 - 2 .. n `div` 2]
