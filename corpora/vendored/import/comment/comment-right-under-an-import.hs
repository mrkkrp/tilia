module Weather.Station (station) where

import Data.Map.Strict (Map)
import Data.Char (toUpper)
-- Station names are upper case.

import Text.Read (readMaybe)
-- Kept for reading observations.

station :: Map String Int
station = mempty
