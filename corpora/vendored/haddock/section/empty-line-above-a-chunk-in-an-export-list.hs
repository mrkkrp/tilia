module Ledger
  ( -- * Accounts

    -- | Opening and closing.
    open,
    close,

    -- | Moving money.
    transfer,
    -- | Undoing a transfer.
    reverse,

    -- * Journals
    -- | Writing it all down.
    record,
  )
where

import Prelude hiding (reverse)

open, close, transfer, reverse, record :: Int
open = 1
close = 2
transfer = 3
reverse = 4
record = 5
