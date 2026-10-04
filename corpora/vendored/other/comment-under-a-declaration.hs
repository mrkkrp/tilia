-- A comment written right under a declaration, with an empty line after
-- it, is a remark on that declaration and stays with it.
module Kiln.Firing where

firingTemperature :: Int
firingTemperature = 1280
-- Cone 10, for stoneware.

coolingRate :: Int
coolingRate = 100
-- Per hour, down to 600.

-- Then left to cool on its own.
soakMinutes :: Int
soakMinutes = 30

-- The same in a class and in an instance.
class Glazed a where
  glaze :: a -> String
  -- Applied before the bisque firing.

  coats :: a -> Int

instance Glazed Int where
  glaze _ = "celadon"
  -- Thin, so that it pools in the carving.

  coats _ = 2

-- After a trailing comment as well.
shelves :: Int
shelves = 4 -- of silicon carbide
-- Kiln wash on the top side only.

posts :: Int
posts = 12

-- With no empty line under it, a comment is about what follows, and an
-- empty line still goes between the two declarations.
cones :: Int
cones = 3
-- Witness cones, one per shelf.
coneHeight :: Int
coneHeight = 2

-- An empty line inside a block comment is no empty line between them.
reduction :: Bool
reduction = True
{- Reduction starts at cone 010,

   and ends with the firing. -}
oxidation :: Bool
oxidation = False
