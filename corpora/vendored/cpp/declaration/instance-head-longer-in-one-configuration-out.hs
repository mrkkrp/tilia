{-# LANGUAGE CPP #-}

module Ledger.Entry where

data Entry account currency amount = Entry account currency amount

instance
#if defined(AUDITED)
  (Show account, Show currency, Show amount) =>
  Show (Entry account currency amount)
  where
#else
  (Show account) => Show (Entry account currency amount) where
#endif
  show _ = "entry"

instance
#if defined(LENIENT)
  (Eq account) => Eq (Entry account currency amount) where
#else
  (Eq account, Eq currency, Eq amount) => Eq (Entry account currency amount)
  where
#endif
  _ == _ = True
