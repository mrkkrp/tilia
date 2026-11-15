module Cache.Report where

import Cache.Entry
  ( Entry
      ( key
        -- the path the entry was read from
      ),
  )
import Cache.Entry
  ( Entry
      ( hits
        -- counted since the last eviction
      ),
  )

describe :: Entry -> String
describe _ = "entry"
