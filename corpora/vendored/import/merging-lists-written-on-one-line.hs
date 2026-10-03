module Billing.Invoice where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Map.Strict (insert, empty)
import qualified Data.Set as Set (Set)
import qualified Data.Set as Set (fromList)

lines' :: Map Text Int
lines' = insert "total" 0 empty
