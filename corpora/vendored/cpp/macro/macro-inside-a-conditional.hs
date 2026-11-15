{-# LANGUAGE CPP #-}

module Lifted.Instances where

#define LIFTED(T) instance Lifted m => Lifted (T m) where lifted = lift . lifted
LIFTED(IdentityT)
#if !MIN_VERSION_transformers(0,6,0)
LIFTED(ListT)
#endif
LIFTED(MaybeT)
#undef LIFTED
