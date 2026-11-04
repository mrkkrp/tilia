module Weather.Station (station) where

import Data.Char (toUpper)
-- Station names are upper case.
import Data.Map.Strict (Map)
import Text.Read (readMaybe)
-- Kept for reading observations.

station :: Map String Int
station = mempty
