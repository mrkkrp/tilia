module Weather.Station (station) where

import Data.Map.Strict (Map)
import Data.Char (toUpper)

-- Reading the observations.

-- readMaybe rejects malformed numbers.
import Text.Read (readMaybe)
import Data.List (sortOn)

station :: Map String Int
station = mempty
