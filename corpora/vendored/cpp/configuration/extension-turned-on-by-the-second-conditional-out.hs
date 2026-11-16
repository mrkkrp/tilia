{-# LANGUAGE CPP #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
#ifdef UNBOXED
{-# LANGUAGE MagicHash #-}
#endif

-- Of the two conditionals above the header, only the second turns on an
-- extension that changes how the rest of the module is read.
module Chimney.Soot (layers) where

#ifdef UNBOXED
import GHC.Exts (Int (I#), (+#))
#endif

layers :: Int -> Int
#ifdef UNBOXED
layers (I# n) = I# (n +# 1#)
#else
layers n = n + 1
#endif
