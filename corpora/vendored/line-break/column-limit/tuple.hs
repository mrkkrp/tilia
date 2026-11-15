-- A tuple written on one line that runs past column 80 is broken, one item
-- per line.
module Loom.Tuples where

counts = (countWarp defaultLoom, countWeft defaultLoom, countSelvedge defaultLoom, shuttleCount)
