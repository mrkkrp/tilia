module Kiln.Glaze where

class Glaze g where
  meltPoint :: g -> Int -- ^ In degrees.
  opaque :: g -> Bool -- ^ Whether it hides the clay.
