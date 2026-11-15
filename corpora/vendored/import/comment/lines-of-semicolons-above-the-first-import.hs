module Survey.Weights where

-- weights are read once and kept for the whole run
;;;; ;;
import Survey.Answer (Answer)
; ; ;
import Data.Map.Strict (Map)

weights :: Map Int Double
weights = mempty
