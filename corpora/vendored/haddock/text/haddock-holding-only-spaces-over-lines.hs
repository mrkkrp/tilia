-- A Haddock written as lines that hold nothing but spaces at their ends
-- says nothing once the spaces go, and the next pass reads it as a
-- comment, so it comes out as one.
module Kiln.Shelf where

data Shelf = Shelf
  { -- |
    -- 
    height :: Int
  }

data Load
  = -- | 
    -- 
    Bisque
  | Glaze
