{-# LANGUAGE CPP #-}

module Garden.Sensor where

import Data.List (sortOn)

-- Only a garden with a weather station keeps a log.

#ifdef STATION
import Garden.Station.Log (readLog)
#endif

readings :: [Int] -> [Int]
readings = sortOn negate
