module Ledger.Account where

data Account = Account
  { owner :: String,
    balance :: Int
  }
  deriving (Eq)
  deriving (Show)

newtype Number = Number Int
  deriving (Eq, Ord)
  deriving (Show)
