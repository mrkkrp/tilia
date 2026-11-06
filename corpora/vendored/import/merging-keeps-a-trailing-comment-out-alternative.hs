module Ledger.Summary where

import Ledger.Posting
  ( postingAccount,
    postingAmount, -- signed, negative for credits
    postingDate,
  )

total :: [Posting] -> Int
total = sum . map postingAmount
