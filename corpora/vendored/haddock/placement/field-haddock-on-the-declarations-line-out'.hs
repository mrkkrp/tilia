module Pottery.Kiln where

data Kiln = Kiln
    { temperature :: Int
      {- ^ In degrees. -}
    }

newtype Shelf = Shelf
    { height :: Int
      {- ^ In centimetres. -}
    }

data Firing
    = Bisque
    | Glaze
        { cone :: Int
          {- ^ How hot. -}
        }
