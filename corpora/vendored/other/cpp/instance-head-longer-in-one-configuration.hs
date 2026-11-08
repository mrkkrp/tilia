{-# LANGUAGE CPP #-}

module Ledger.Entry where

data Entry account currency amount = Entry account currency amount

#if defined(AUDITED)
instance (Show account, Show currency, Show amount) => Show (Entry account currency amount) where
#else
instance (Show account) => Show (Entry account currency amount) where
#endif
  show _ = "entry"

#if defined(LENIENT)
instance (Eq account) => Eq (Entry account currency amount) where
#else
instance (Eq account, Eq currency, Eq amount) => Eq (Entry account currency amount) where
#endif
  _ == _ = True
