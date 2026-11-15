{-# LANGUAGE PatternSynonyms #-}

-- A comment written beside the wildcard of an item stays inside its
-- parentheses.
module Ledger.Entry
  ( Entry({- fields too -}..)
  , Side(Debit, {- and the rest -} ..)
  , total
  ) where

import Ledger.Money (Money(.. {- abstract upstream -}), add)

data Entry = Entry { entrySide :: Side, entryAmount :: Money }

data Side = Debit | Credit

total :: [Entry] -> Money
total = foldr (add . entryAmount) mempty
