module Billing.Tax where

import Billing.Rate
  ( Rate,
    rateFor,
    rateName,
  )
import Billing.Region
  ( Region,
    regionCode,
    regionName,
  )
import Data.Map
  ( Map,
    empty,
    lookup,
    member,
  )

tax :: Region -> Double
tax _ = 0
