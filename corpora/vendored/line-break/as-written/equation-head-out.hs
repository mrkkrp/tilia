-- The head of an equation keeps its line however long, since its patterns
-- are indented only where the author wrote them over several lines.
module Loom.Heads where

weave (Loom name warp weft tension) (Pattern repeats colours) (Shuttle speed load) threads =
  name
