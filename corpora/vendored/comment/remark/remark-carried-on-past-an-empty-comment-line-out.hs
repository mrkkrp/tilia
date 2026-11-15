module Courier.Retry where

timeout :: Int
timeout = 30 -- seconds
             --
             -- the depot gives up at sixty

data Policy = Policy
  { retries :: Int, -- per parcel
                    --
                    -- doubled on a cold start
    backoff :: Int
  }

delays :: [Int]
delays =
  [ 1,
    2, -- doubled
       --
       -- then capped
    4
  ]
