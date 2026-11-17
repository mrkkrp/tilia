{-# LANGUAGE CPP #-}
#ifdef PORTABLE
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE MagicHash #-}
#endif
module Counter where

import GHC.Exts

step :: Int -> Int
#ifndef PORTABLE
step (I# n) = I# (n +# 1#)
#else
step n = n+1
#endif
