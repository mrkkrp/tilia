module Range.Bounds where

width :: Range -> Int
width range = high - low
  where
    Range low high = range
    Bounds
      lower
      upper = limits range

clamped :: Range -> Int -> Int
clamped range = max low . min high
  where
    Range low high = range
