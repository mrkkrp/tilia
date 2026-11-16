{-# LANGUAGE CPP #-}
#ifdef BRUSH_SIZE
{-# LANGUAGE MagicHash #-}
#endif

-- A macro that is not defined stands for 0 where it is compared with a
-- number, so where the first conditional fails, the last one fails too.
module Chimney.Brush (bristles) where

#ifdef BRUSH_SIZE
import GHC.Exts (Int (I#), (*#))
#endif

bristles :: Int -> Int
#if BRUSH_SIZE >= 3
bristles (I# n) = I# (n *# 40#)
#else
bristles n = n*20
#endif
