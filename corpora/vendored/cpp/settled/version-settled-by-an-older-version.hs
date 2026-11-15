{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,10,0)
{-# LANGUAGE MagicHash #-}
#endif
module Words where

#if MIN_VERSION_base(4,11,0)
import GHC.Exts (Int (I#), (+#))
#endif

#if MIN_VERSION_base(4,11,0)
plusOne :: Int -> Int
plusOne (I# n) = I# (n +# 1#)
#else
plusOne :: Int -> Int
plusOne n = n+1
#endif
