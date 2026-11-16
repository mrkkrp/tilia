{- |
Raised beds and what grows in them.

Each bed is a grid of squares, one plant to a square.
-}
{-# LANGUAGE DerivingStrategies #-}
-- the grid stays square until beds can be irregular
module Garden.Beds (Bed, plant) where

data Bed = Bed [[Maybe String]]
  deriving stock (Show)

plant :: String -> Int -> Int -> Bed -> Bed
plant _ _ _ = id
