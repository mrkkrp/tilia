module Ledger.Rules where

import Ledger.Rule
  ( ruleName,
    rulePriority,
  )
import Ledger.Rule
  ( ruleMatcher,
    -- matched against the whole description
  )

named :: Rule -> String
named = ruleName
