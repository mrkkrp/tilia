{-# LANGUAGE CPP #-}

module Ledger.Compat (module X) where

#if MIN_VERSION_base(4,19,0)
import Data.List as X (unsnoc)
#else
import Ledger.Compat.List as X (unsnoc)
#endif
import Data.Functor   as X  ( (<&>) )
