-- One-line declarations of one kind written together keep their lines
-- however long, so that a run stays a run, and one standing apart breaks.
module Loom.Run where

tension = 3
weave threads thread colour = summarise (findWithDefault colour thread threads) (count threads)
shuttleCount = 2

warp threads thread colour =
  summarise (findWithDefault colour thread threads) (count threads)
