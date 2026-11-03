-- A local binding that runs past column 80 is broken after its =, also
-- where it is written on the line of its where or let.
module Loom.LocalBindings where

selvedge loom = finish edgeThreads loom
  where
    edgeThreads =
      take (selvedgeWidth loom) (dropWhile (not . isEdge) (loomWarp loom))

bobbins threads = do
  let spun =
        spinAll (filter (not . frayed) threads) (length threads) (twistOf loom)
  pure spun
