{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,10,0)
{-# LANGUAGE MagicHash #-}
#endif
module Raise (raiseIt, sorted) where

import Control.Exception
#if MIN_VERSION_base(4,10,0) && !MIN_VERSION_base(4,12,0)
  hiding (throw)
import GHC.Exts (raise#)
#endif
import Data.List (sort)

#if MIN_VERSION_base(4,10,0)
raiseIt :: SomeException -> a
raiseIt = raise#
#endif

sorted :: [Int] -> [Int]
sorted = sort
