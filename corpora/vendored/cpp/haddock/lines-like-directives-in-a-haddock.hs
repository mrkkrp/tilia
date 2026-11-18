{-# LANGUAGE CPP #-}

module Store where

class Store s where
  {- | Not yet:
  #ifdef WITH_LOCKS
  lock :: s -> IO ()
  #endif
  -}
  open :: s

  {- | Closes the store
#ifdef WITH_LOCKS
#ifdef WITH_TIMEOUTS
  once the lock times out
#endif
#endif
  -}
  close :: s -> IO ()
