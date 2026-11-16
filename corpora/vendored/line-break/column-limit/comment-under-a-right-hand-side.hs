-- A right-hand side that runs past column 80 with a comment lined up under
-- it is broken where it starts a line of its own, and the comment stays
-- lined up with its start.
module Ledger.Match where

instance Eq Ledger where
  a == b = ledgerName a == ledgerName b && ledgerOwner a == ledgerOwner b && ledgerYear a == ledgerYear b -- names only
           -- the rest follows from them
