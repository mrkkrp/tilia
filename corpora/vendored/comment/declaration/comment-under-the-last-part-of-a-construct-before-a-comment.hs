-- A comment lined up with the last line of a construct stays inside it when
-- a comment heads what follows, as it does when code follows.
module Pottery.Wheel where

class Thrown a where
  centre :: a -> a
  -- with wet hands

-- Anticlockwise.
speed :: Int -> Int
speed clay
  | clay > 5 = 120
  | otherwise = 90
  -- in turns a minute

{- Note [Trimming]
~~~~~~~~~~~~~~~~~
Trimmed when leather-hard.
-}
trim :: Maybe Int -> Int
trim depth = case depth of
  Just d -> d
  Nothing -> 2
  -- millimetres off the foot
-- Right under it, too.
dry :: Int
dry = days
  where
    days = 7
    -- longer in winter

{- A block comment. -}
data Body = Stoneware | Porcelain
  deriving Eq
  -- by what fires it

-- Over several lines,
-- and then another.

-- The second one.
pots :: Int
pots = shelves * 12
       -- after the bisque firing

-- On four shelves.
shelves :: Int
shelves =
  4
  -- or five for small pots

-- | A Haddock did not change it.
glaze :: Int
glaze = 3
