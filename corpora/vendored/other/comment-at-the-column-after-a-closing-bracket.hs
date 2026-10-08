module Loom.Warp where

warpThreads :: Int
warpThreads = (count 1)
                       -- leaves out the selvedge

weftThreads :: [Int]
weftThreads = [count 2]
                       -- the same in a list

shedHeight :: Int
shedHeight =
  lift
    treadle
    (count 3)
             -- and in the last argument

reedDents :: Int
reedDents = 12
