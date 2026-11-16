{-# OPTIONS_GHC -fno-implicit-prelude #-}

module Garden.Pairs where

import Data.Tuple (swap)
import Garden.Base (Bool (..))
import Prelude (Int)
import System.IO (print)

-- The old flag turns the implicit Prelude off, so the Prelude is sorted
-- among the other imports rather than put after them.
paired :: Int -> (Int, Bool)
paired n = (n, True)
