{-# LANGUAGE CPP #-}

module Network.Call where

-- The Haddock after the record documents its last field where that is the
-- optional one, and nothing where it is not.
data Call
  = Call
    { procedure :: Int -- ^ The procedure.
#ifdef RESERVED_PORT
    , reserved :: Bool
#endif
    } -- ^ A call.
