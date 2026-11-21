{-# LANGUAGE CPP #-}

-- A macro may stand for more constraints than one, so the brackets written
-- around a context headed by one stay.
module Control.Monad.Trans.Fail where

import Data.String (IsString)

#if !MIN_VERSION_base(4,13,0)
#define IS_MONAD_STRING IsString e,
#else
#define IS_MONAD_STRING
#endif

failing :: (IS_MONAD_STRING Monad m) => e -> m ()
failing _ = pure ()

class Monad m => MonadFailure e m where
  failure :: e -> m a

instance (IS_MONAD_STRING Monad m) => MonadFailure e (Maybe' m) where
  failure _ = undefined
