module Inventory.Index where

import Data.Map hiding (filter, map) -- the Prelude's are the ones meant below

index :: [(String, Int)] -> Map String Int
index = fromList . map id . filter ((> 0) . snd)
