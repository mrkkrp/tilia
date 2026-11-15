{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitForAll #-}

module Kiln.Clock where

-- The binders of a forall that a conditional varies whole stay in
-- each branch, on a line of their own.
lookupCont
#if defined(__GLASGOW_HASKELL__)
    :: forall r k v
#else
    :: forall k v r
#endif
     . k -> (v -> r) -> r
lookupCont = undefined
