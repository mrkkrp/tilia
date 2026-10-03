module Inventory.Stock where

import Data.List (sortOn)
import Data.Map.Strict (Map, adjust, findWithDefault)
import qualified Data.Set as Set (Set, member)
import Data.Text (Text)

inStock :: Text -> Map Text Int -> Bool
inStock item = (> 0) . findWithDefault 0 item
