{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Ledger.Amount where

import Data.Monoid (Sum (..))

newtype Amount = Amount Int deriving newtype (Num)

newtype Credit = Credit Int
  deriving (Semigroup, Monoid) via (Sum Int)

data Entry = Entry Amount String
  deriving stock (Show)

data Note = Note String
  deriving anyclass (Describable)
