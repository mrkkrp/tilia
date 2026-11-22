{-# LANGUAGE CPP #-}

-- A comment set apart above the imports of a branch heads them, as at the
-- top of a module, where no import comes above it.
module Kiln.Firing (fire) where

#if defined(CONE_TEN)
import Kiln.Stoneware (fire)
#elif defined(CONE_SIX)
-- Mid-fire.

import Data.Maybe (fromMaybe)
import Data.Char (toUpper)

fire :: Int
fire = 6
#else
-- Earthenware.

import Data.Maybe (fromMaybe)
import Data.Char (toUpper)
#endif
