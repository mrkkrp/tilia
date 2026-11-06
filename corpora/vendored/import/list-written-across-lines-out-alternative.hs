module Inventory.Ledger where

import Data.Map.Strict
  ( Map,
    insertWith,
  )
import Data.Sequence
  ( Seq,
    empty,
    fromList,
  )
import Data.Text
  ( Text,
    pack,
  )
import Data.Time
  ( UTCTime,
    getCurrentTime,
  )

record :: Text -> Int -> Map Text Int -> Map Text Int
record = insertWith (+)
