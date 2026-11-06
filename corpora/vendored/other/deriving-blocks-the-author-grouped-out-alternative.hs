{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ledger.Posting where

newtype Tallied a = Tallied a

deriving via Tallied Receipt instance Eq Receipt
deriving via Tallied Receipt instance Ord Receipt
deriving via Tallied Receipt instance Show Receipt

deriving via Tallied Invoice instance Eq Invoice
deriving via Tallied Invoice instance Ord Invoice
-- A note written against the block it stands in, and staying inside it.
deriving via Tallied Invoice instance Show Invoice

deriving via Tallied Memo instance Eq Memo
