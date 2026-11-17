{-# LANGUAGE CPP #-}
#if defined USE_FFI
{-# LANGUAGE MagicHash #-}
#endif

module Counter where

step :: Int -> Int
#if defined(USE_FFI)
step (I# n) = I# (n +# 1#)
#else
step n = n + 1
#endif
