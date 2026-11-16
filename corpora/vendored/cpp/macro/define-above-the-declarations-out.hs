{-# LANGUAGE CPP #-}

module Lifted.Instances (lift) where

#define LIFTED(T) instance Lifted m => Lifted (T m) where lifted = lift . lifted

lift :: m a -> t m a
lift = undefined

#if !MIN_VERSION_transformers(0,6,0)
LIFTED(ListT)
#endif

lifted :: m a -> m a
lifted = id
