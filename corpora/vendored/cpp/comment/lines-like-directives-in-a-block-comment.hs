{-# LANGUAGE CPP #-}

module Store where

#ifdef DEBUG
import Debug.Trace
#endif

class Store s where
  {- Not yet:
  #ifdef WITH_LOCKS
  lock :: s -> IO ()
  #endif
  -}
  open :: s

backoff :: Int -> Int
backoff n  =  n * 2 -- grows with each try
                    {- and is capped
                    #ifdef WITH_DEPOT
                       by the depot
                    #endif
                     -}
