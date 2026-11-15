{-# LANGUAGE CPP #-}

-- An empty line next to a directive is the preprocessor support's to
-- place: it goes under the #endif, and the comment stays under the
-- declaration it was written under.
module Kiln.Firing where

#if MIN_VERSION_base(4,0,0)
firingTemperature :: Int
firingTemperature = 1280
-- Cone 10, for stoneware.

#endif
coolingRate :: Int
coolingRate = 100
