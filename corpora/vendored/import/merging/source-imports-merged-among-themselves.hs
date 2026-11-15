module Ledger.Posting where

import Ledger.Types (Balance)
import {-# SOURCE #-} Ledger.Types (Account)
import {-#SOURCE#-} qualified Ledger.Journal as Journal
import {-# SOURCE #-} Ledger.Types (Currency)

post :: Account -> Balance -> Journal.Entry
post = undefined
