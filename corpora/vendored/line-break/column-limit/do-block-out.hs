-- A do block written on one line that runs past column 80 is broken, one
-- statement per line.
module Loom.Blocks where

weave loom = do
  threads <- spinAll (loomWarp loom)
  tighten threads
  pure (length threads)
