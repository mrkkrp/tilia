module Ledger
  ( open
  -- Kept for the old journal format.

  , close
  -- Right above the next one.
  , record
  -- After a comma.
  , reopen
  )
where

data Entry = Credit | Debit | Fee | Refund | Void

open, close, record, reopen :: [Entry]
open =
  [ Credit
  -- Kept for the old journal format.

  , Debit
  -- Right above the next one.
  , Fee
  -- After a comma.
  , Refund
  , {- in place -} Void
  ]
close = open
record = open
reopen = open
