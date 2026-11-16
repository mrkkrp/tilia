module Queue.Priority where

import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

bumped :: Int -> Seq Int -> Seq Int
bumped n queue =
    n + 1
        Seq.<| n * 2
        Seq.<| queue

lookedUp :: Map.Map String Int -> Bool
lookedUp table =
    table Map.! "head" + 1
        <= Map.size table
        && Map.member "tail" table

audited :: Report -> Bool
audited r =
    r `Audit.passes` strict && r `Audit.within` 3
        || r `Audit.waived` True
