{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 904
{-# LANGUAGE MagicHash #-}
#endif

-- An answer to >= settles the comparisons with <, <= and != that follow
-- from it, so no configuration takes the code written for MagicHash
-- without the pragma that turns it on.
module Counter.Unboxed where

#if __GLASGOW_HASKELL__ >= 904
import GHC.Exts (Int (I#), (*#), (+#))
#endif

step :: Int -> Int
#if __GLASGOW_HASKELL__ < 906
step n = n + 1
#else
step (I# n) = I# (n +# 1#)
#endif

twice :: Int -> Int
#if __GLASGOW_HASKELL__ <= 905
twice n = n * 2
#else
twice (I# n) = I# (n *# 2#)
#endif

nine :: Int
#if __GLASGOW_HASKELL__ != 906
nine = 9
#else
nine = I# 9#
#endif
