module Ledger.Sync where

import Control.Applicative ((<|>))

sync :: Ledger -> IO ()
sync ledger = retrying $ do pushEntries ledger
  <|> queueForLater ledger
