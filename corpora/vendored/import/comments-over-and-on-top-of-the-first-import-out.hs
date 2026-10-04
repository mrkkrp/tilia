module Survey.Weights where

-- weights are read once and kept for the whole run

import Data.IORef (IORef, newIORef)
import Data.Map.Strict (Map)
-- answers are matched to their questions by position
import Survey.Answer (Answer)

weights :: IO (IORef (Map Int Double))
weights = newIORef mempty
