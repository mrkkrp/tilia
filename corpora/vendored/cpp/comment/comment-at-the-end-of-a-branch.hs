{-# LANGUAGE CPP #-}

module Terminal.Timing where

timeout :: Int
#ifdef SLOW_LINK
timeout  =  30000
-- A serial line needs a great deal longer than a pipe does.
#else
timeout  =  250
#endif

retries :: Int
retries  =  3
