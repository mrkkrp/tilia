module Shape where

data Corner
  = -- | Drawn with an arc.
    Rounded

data Edge = Straight

width
  :: Int
  -- ^ In pixels.
width = 4

height = 3
