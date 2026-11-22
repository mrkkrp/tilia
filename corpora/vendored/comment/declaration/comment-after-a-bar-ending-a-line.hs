module Ledger.Entry where

-- A comment after a bar that ends its line stays on that line.
data Entry = Debit Int | -- money out
             Credit Int | -- money in
             Transfer Int Int

-- A comment after a bar that begins its line stays after the bar.
data Account
  = Cash
  | -- held at the bank
    Bank
  | Card
