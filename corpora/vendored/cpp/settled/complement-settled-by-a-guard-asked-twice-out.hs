{-# LANGUAGE CPP #-}
#if API_LEVEL >= 3
{-# LANGUAGE MagicHash #-}
#endif

module Counter where

#if API_LEVEL >= 3
import GHC.Exts (Int (I#), (+#))
#endif

step :: Int -> Int
#if API_LEVEL < 3
step n = n + 1
#else
step (I# n) = I# (n +# 1#)
#endif
