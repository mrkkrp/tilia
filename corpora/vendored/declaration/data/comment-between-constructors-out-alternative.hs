module Kiln.Glaze where

-- A comment between constructors keeps the empty lines the author left
-- around it, but for one above it when it joins the line of a bar.
data Finish
  = Matte
  | -- Dry to the touch.

    Satin
  | -- Somewhere between the two.

    Gloss

-- A Haddock belongs to its constructor, and no empty line goes around it.
data Kiln
  = Electric
  | -- | Fired with gas, for reduction.
    Gas
  | Wood

-- The same between the constructors of a GADT.
data Firing a where
  Bisque :: Firing ()
  -- The first firing, unglazed.

  Glaze :: Firing Finish
  -- | Fired once more, low.
  Luster :: Firing Finish
