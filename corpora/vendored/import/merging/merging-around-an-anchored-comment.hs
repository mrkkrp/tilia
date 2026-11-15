module Ledger.Rules where

import Ledger.Rule (ruleName)
import Ledger.Rule
  ( ruleMatcher
  -- matched against the whole description
  )
import Ledger.Rule (rulePriority)

named :: Rule -> String
named = ruleName
