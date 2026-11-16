module Ledger.Legacy {-# WARNING in "x-legacy-ledger"
  "Balances here ignore pending transfers"
  #-} (balance) where

balance :: [Int] -> Int
balance = sum
