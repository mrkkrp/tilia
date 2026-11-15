{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE MagicHash #-}
#endif
module Unboxed where

import GHC.Exts

#if __GLASGOW_HASKELL__ >= 802
unbox :: Int -> Int#
unbox (I# n) = n
#endif

#if __GLASGOW_HASKELL__ > 900
twice :: Int# -> Int#
twice n = n *# 2#
#else
twice :: Int -> Int
twice n = n*2
#endif
