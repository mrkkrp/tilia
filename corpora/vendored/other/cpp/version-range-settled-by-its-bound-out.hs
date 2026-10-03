{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,10,0)
{-# LANGUAGE MagicHash #-}
#endif

module Raise (raiseIt, sorted) where

#if MIN_VERSION_base(4,10,0) && !MIN_VERSION_base(4,12,0)
import Control.Exception hiding (throw)
import GHC.Exts (raise#)
#else
import Control.Exception
#endif
import Data.List (sort)

#if MIN_VERSION_base(4,10,0)
raiseIt :: SomeException -> a
raiseIt = raise#
#endif

sorted :: [Int] -> [Int]
sorted = sort
