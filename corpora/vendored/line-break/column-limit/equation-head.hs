-- The head of an equation that runs past column 80 puts each of its
-- patterns on a line of its own, a step in from the name, and what follows
-- the head goes a step past the patterns.
module Loom.Heads where

weave (Loom name warp weft tension) (Pattern repeats colours) (Shuttle speed load) threads = name

(Loom name warp weft tension) `interlace` (Pattern repeats colours shuttleSpeed bobbin) = name

tension (Loom name warp weft tension) (Pattern repeats colours) (Shuttle speed load)
  | speed > load = warp
  | otherwise = weft

instance Woven Loom where
  woven (Loom name warp weft tension) (Pattern repeats colours) (Shuttle speed load) = do
    spin name
    pure warp
