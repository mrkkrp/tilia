module Pottery.Kiln where

data Kiln = Kiln
  { -- | In degrees.
    temperature :: Int
  }

newtype Shelf = Shelf
  { -- | In centimetres.
    height :: Int
  }

data Firing
  = Bisque
  | Glaze
      { -- | How hot.
        cone :: Int
      }
