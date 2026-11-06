{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,10,0)
{-# LANGUAGE MagicHash #-}
#endif

module Words where

import Data.Bits (shiftL)
#if MIN_VERSION_base(4,11,0)
import GHC.Exts (Int (I#), uncheckedIShiftL#)

double :: Int -> Int
double (I# n) = I# (uncheckedIShiftL# n 1#)
#endif
