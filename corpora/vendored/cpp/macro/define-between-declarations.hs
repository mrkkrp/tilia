{-# LANGUAGE CPP #-}

module Lifted.Instances where

lift :: m a -> t m a
lift = undefined

#define LIFTED(T) instance Lifted m => Lifted (T m) where lifted = lift . lifted

lifted :: m a -> m a
lifted = id

#if !MIN_VERSION_transformers(0,6,0)
LIFTED(ListT)
#endif
