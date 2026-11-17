{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecursiveDo #-}

module Ledger.Posting where

import qualified Ledger.Journal as J

post :: Entry -> J.Journal ()
post entry = J.do
  account <- J.lookup (entryAccount entry)
  J.append account entry

postBoth :: Entry -> Entry -> J.Journal ()
postBoth a b = J.do { post a; post b }

settle :: Entry -> J.Journal ()
settle entry = J.mdo
  balance <- J.adjust total entry
  total <- J.sumOf balance
  J.close total
