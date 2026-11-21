{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

#if __GLASGOW_HASKELL__ >= 704
#define HasCallStack_ HasCallStack =>
#else
#define HasCallStack_
#endif

module Assert (assertFailure) where

import GHC.Stack (HasCallStack)

-- | Signals a failure.
assertFailure ::
     HasCallStack_
     String -- ^ A message that is displayed with the assertion failure
  -> IO a
assertFailure = ioError . userError
