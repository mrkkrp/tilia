module Inventory.Count where

import Data.Map.Strict (Map, {- strict in the counts -} insertWith)

bump :: String -> Map String Int -> Map String Int
bump item = insertWith (+) item 1
