{-# LANGUAGE DerivingStrategies #-}

module Shop.Item where

-- A Haddock above a deriving clause documents the first class in it, but
-- above a strategy it documents nothing.
data Item = Item String Int
  -- | Compared by name, then by price.
  deriving (Eq, Ord)

data Size = Small | Large
  -- | As the catalogue prints it.
  deriving Show

data Colour
  = Red
  | Blue
  -- | As the catalogue prints it.
  deriving stock Show
