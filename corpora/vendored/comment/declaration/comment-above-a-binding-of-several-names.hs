module Garden.Beds where

-- Both take the width of a bed in centimetres.
sow, weed :: Int -> Int
sow width = width `div` 10
weed width = width `div` 30

-- Taken apart once, at the top level.
whole@(rows, columns) = (4, 12)
