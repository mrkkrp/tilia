-- A function application that runs past column 80 even on a line of its own
-- is broken, one argument per line.
module Loom.Applications where

bobbins threads =
  arrangeOnBeam
    (reverse (sortOn tension threads))
    (beamWidth defaultLoom)
    (beamDepth defaultLoom)
    2
