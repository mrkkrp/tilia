module Ledger.Summary where

import Ledger.Posting (postingDate)
import Ledger.Posting
  ( postingAccount,
    postingAmount -- signed, negative for credits
  )

total :: [Posting] -> Int
total = sum . map postingAmount
