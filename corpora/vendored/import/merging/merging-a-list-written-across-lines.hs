module Billing.Tax where

import Billing.Rate (Rate)
import Billing.Rate
  ( rateFor,
    rateName
  )
import Billing.Region
  ( Region,
    regionCode
  )
import Billing.Region (regionName)
import Data.Map (Map)
import Data.Map
  ( lookup,
    member
  )
import Data.Map (empty)

tax :: Region -> Double
tax _ = 0
