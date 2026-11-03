-- A signature naming one thing that runs past column 80 is broken after its
-- :: and its =>, at the top level, in a class and among local bindings.
module Loom.Signatures where

weave :: (Ord thread, Show colour) => Map thread colour -> thread -> colour -> Either String Int
weave = undefined

class Shuttle shuttle where
  throw :: (Ord thread, Show colour) => shuttle -> Map thread colour -> thread -> Either String Int

selvedge loom = edgeThreads loom
  where
    edgeThreads :: (Ord thread) => Loom thread -> Map thread Colour -> [(thread, Colour)]
    edgeThreads = undefined
