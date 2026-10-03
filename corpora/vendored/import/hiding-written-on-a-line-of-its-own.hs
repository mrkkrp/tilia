module Inventory.Search where

import Data.Map
  hiding (filter, map)
import Data.Set hiding
  ( filter,
    map
  )
import qualified Data.Text as T
  hiding (null)

matching :: (k -> Bool) -> Map k v -> [k]
matching p = Prelude.filter p . keys
