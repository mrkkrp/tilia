module Survey.Weights where

import Data.Map.Strict (Map)
-- weights are read once and kept for the whole run
import Survey.Answer (Answer)

weights :: Map Int Double
weights = mempty
