module Sensor.Calibration where

import Data.List.NonEmpty (NonEmpty (..))

offset :: Double
gain :: Double
(offset, gain) = (0.5, 1.2)

lastReading :: Int
firstReading :| [lastReading] = 3 :| [4]

threshold :: Double

(low, high) = (0.1, 0.9)
threshold = 0.5
