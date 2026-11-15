{-# LANGUAGE CPP #-}
#if defined(UNBOXED_TICKS)
{-# LANGUAGE MagicHash #-}
#endif

module Kiln.Clock where

#if defined(UNBOXED_TICKS)
import GHC.Exts (Int#, (+#))
#endif

#if 0
tick :: Int -> Int
tick n = n + 2
#else
tick :: Int -> Int
tick n = n + 1
#endif

#if 1
seconds :: Int -> Int
seconds t = t * 60
#endif
