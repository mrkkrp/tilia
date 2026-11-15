module Inventory.Index where

import Data.Map hiding -- the Prelude's are the ones meant below
  ( filter,
    map,
  )

index :: [(String, Int)] -> Map String Int
index = fromList . map id . filter ((> 0) . snd)
