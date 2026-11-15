module Grade.Letter where

letter :: Int -> Char
letter score -- out of a hundred
    | score >= 90 = 'A'
    | score >= 75 = 'B'
    | otherwise = 'C'
