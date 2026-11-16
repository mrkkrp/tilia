module Kiln.Glaze where

class Glaze g where
  meltPoint ::
    g ->
    -- | In degrees.
    Int
  opaque ::
    g ->
    -- | Whether it hides the clay.
    Bool
