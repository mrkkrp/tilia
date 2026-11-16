module Stock.Shelves where

import Data.Map (Map)

shelfTotals :: [Map String Int] -> [Int]
shelfTotals = map (\(shelf :: Map String Int) -> sum shelf)

fullest :: Map String Int -> Maybe Int
fullest (shelf :: Map String Int) = maximum (Just <$> shelf)
