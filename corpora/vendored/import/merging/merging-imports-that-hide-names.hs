module Library.Catalogue where

import Data.Map.Strict hiding (insert)
import Data.Text (Text)
import Data.Map.Strict hiding (delete)
import Data.Map.Strict (insert)
import qualified Data.Set as Set hiding (map, filter)
import Data.Map.Strict (delete)
import qualified Data.Set as Set hiding
  ( filter,
    map,
    map
  )
import Library.Shelf hiding (Shelf (Top), shelfName)
import Library.Shelf hiding (shelfName, Shelf (Bottom, Top))
import Library.Shelf hiding (Shelf (Top, Bottom), shelfName)
import Data.Map.Strict hiding (insert)

shelve :: Text -> Map Text Int -> Map Text Int
shelve title = insert title 1 . delete title
