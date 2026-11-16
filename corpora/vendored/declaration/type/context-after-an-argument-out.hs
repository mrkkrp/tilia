module Stats.Summary where

summarise :: (Ord a) => [a] -> (Show a) => String
summarise xs = show (maximum xs)

spread ::
  (Num a) =>
  [a] ->
  [a] ->
  (Ord a) =>
  (Show a) =>
  [a] -> String
spread lo hi xs = show (zipWith3 between lo hi xs)
