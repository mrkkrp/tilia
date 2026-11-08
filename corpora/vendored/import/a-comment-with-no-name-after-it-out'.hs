module Ledger.Journal where

import Ledger.Entry (entryDate)
import Ledger.Entry
  ( entryNarration
  -- kept as written, trailing spaces and all
  )

dated :: Entry -> Int
dated = entryDate
