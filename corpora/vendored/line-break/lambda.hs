-- A lambda that runs past column 80 is broken after its arrow.
module Loom.Lambdas where

rethreader = \loom -> rewind (reverse (loomWarp loom)) (loomTension loom + 3) (spoolsOf defaultLoom)
