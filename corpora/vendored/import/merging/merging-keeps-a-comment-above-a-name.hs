module Ledger.Balance where

import Ledger.Account (accountName)
import Ledger.Account
  ( -- carried in from the period before this one
    accountOpening,
    accountKind
  )

label :: Account -> String
label = accountName
