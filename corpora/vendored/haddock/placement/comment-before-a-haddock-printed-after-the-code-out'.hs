{-# LANGUAGE GADTs #-}

module Kiln.Glaze where

class Glaze g where
    meltPoint
        :: g
        -> Int -- in degrees
        -- ^ How hot it has to get.
    opaque
        :: g
        -> Bool -- after one firing
        -- ^ Whether it hides the clay.

data Batch where
    Seconds
        :: Int
        -> Batch -- unsold
        -- ^ What is left.

class Narrowed a b | a -> b -- one glaze to a kiln
    -- | a class with no body
