{-# LANGUAGE CPP #-}

-- The empty line above the first import stays at the top of the imports
-- when sorting moves that import down, as without a conditional.
module Kiln.Schedule where

import Data.List (sortOn)
-- For the ramp.
import Data.Maybe (fromMaybe)

ramp :: Int
ramp = 100

#ifdef CONE_TEN
hold :: Int
hold = 15
#endif
