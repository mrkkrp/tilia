{-# LANGUAGE CPP #-}
module B2 where

import Data.Maybe (fromMaybe)
#ifdef X
import Data.List (sort)

sorted :: [Int] -> [Int]
sorted = sort
#else
import Data.Char (ord)
#endif
