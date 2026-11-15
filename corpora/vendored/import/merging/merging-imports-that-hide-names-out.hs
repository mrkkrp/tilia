module Library.Catalogue where

import Data.Map.Strict (delete, insert)
import Data.Map.Strict hiding (delete)
import Data.Map.Strict hiding (insert)
import qualified Data.Set as Set hiding
  ( filter,
    map,
  )
import Data.Text (Text)
import Library.Shelf hiding (Shelf (Bottom, Top), shelfName)
import Library.Shelf hiding (Shelf (Top), shelfName)

shelve :: Text -> Map Text Int -> Map Text Int
shelve title = insert title 1 . delete title
