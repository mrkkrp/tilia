{-# LANGUAGE CPP #-}

module Terminal.Timing where

-- | How long to wait for a response.
timeout :: Int
#ifdef SLOW_LINK
-- A serial line needs a great deal longer than a pipe does.
timeout  =  30000 -- milliseconds
#else
timeout  =  250
#endif

-- | Retries before giving up.
retries :: Int
retries  =  3
