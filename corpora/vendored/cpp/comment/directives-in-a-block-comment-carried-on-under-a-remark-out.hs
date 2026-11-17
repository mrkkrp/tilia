{-# LANGUAGE CPP #-}

module Courier where

backoff :: Int -> Int
backoff n = n * 2 -- grows with each try
                  {- and is capped
#if 0
                     by the depot
#endif
                   -}

pause :: Int
pause = wait
  where
    wait = backoff 3 -- before the next try
    {- in seconds,
#ifdef SLOW_LINK
       at most a minute
#else
       at most ten
#endif
     -}
