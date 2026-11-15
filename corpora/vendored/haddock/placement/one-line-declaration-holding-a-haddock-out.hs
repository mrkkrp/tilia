module Shape where

data Corner
  = -- | Drawn with an arc.
    Rounded

data Edge = Straight

width ::
  -- | In pixels.
  Int
width = 4

height = 3
