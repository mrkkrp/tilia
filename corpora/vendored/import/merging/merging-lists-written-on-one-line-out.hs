module Billing.Invoice where

import Data.Map.Strict (Map, empty, insert)
import qualified Data.Set as Set (Set, fromList)
import Data.Text (Text)

lines' :: Map Text Int
lines' = insert "total" 0 empty
