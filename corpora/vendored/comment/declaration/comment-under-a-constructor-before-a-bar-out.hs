-- A comment lined up with the last line of a constructor, rather than with
-- the bar under it, stays with that constructor.
module Kiln.Shelf where

data Shelf
  = Corner
      Int
      -- posts at each end
  | Half
    -- lined up with the constructor itself
  | Full
      Int
      Int
      -- one post in the middle as well

  | -- | A shelf cut to fit.
    Bat
      { width :: Int,
        depth :: Int
      }
      -- measured on the kiln floor
  | Broken
  deriving (Show)
