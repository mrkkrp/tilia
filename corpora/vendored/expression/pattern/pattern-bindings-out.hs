module Range.Split where

(low, high) = (1, 99)

middle :: Int
middle = let (q, _) = (low + high) `divMod` 2 in q

halves :: [a] -> ([a], [a])
halves xs = (front, back)
  where
    (front, back) = splitAt (length xs `div` 2) xs
