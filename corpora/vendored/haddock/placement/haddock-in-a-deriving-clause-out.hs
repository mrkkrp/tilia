module Shape.Kind where

-- A Haddock on a class in a deriving clause stays in front of it, rather
-- than going under it as the Haddock of an argument does.
data Kind = Polygon | Curve deriving ({- | how they are shown -} Show, Read)
