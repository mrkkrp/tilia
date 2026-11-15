-- A signature naming several things that runs past column 80 is broken
-- after its names, one per line, and after its :: where its type does not
-- fit.
module Loom.Signatures where

tension,
  shuttleCount,
  spoolLength,
  beamWidth,
  beamDepth,
  warpCount :: Loom -> Int
