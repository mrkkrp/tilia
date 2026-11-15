module Ledger.Posting where

import {-# SOURCE #-} qualified Ledger.Journal as Journal
import Ledger.Types (Balance)
import {-# SOURCE #-} Ledger.Types (Account, Currency)

post :: Account -> Balance -> Journal.Entry
post = undefined
