module Weather.Station (station) where

import Data.Char (toUpper)
import Data.Map.Strict (Map)

-- Reading the observations.

import Data.List (sortOn)
-- readMaybe rejects malformed numbers.
import Text.Read (readMaybe)

station :: Map String Int
station = mempty
