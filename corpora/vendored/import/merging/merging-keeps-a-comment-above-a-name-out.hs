module Ledger.Balance where

import Ledger.Account
  ( accountKind,
    accountName,
    -- carried in from the period before this one
    accountOpening,
  )

label :: Account -> String
label = accountName
