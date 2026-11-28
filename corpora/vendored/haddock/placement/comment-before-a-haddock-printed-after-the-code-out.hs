{-# LANGUAGE GADTs #-}

module Kiln.Glaze where

class Glaze g where
  meltPoint ::
    g ->
    -- | How hot it has to get.
    Int -- in degrees
  opaque ::
    g ->
    -- | Whether it hides the clay.
    Bool -- after one firing

data Batch where
  Seconds :: Int -> {- | What is left. -} Batch -- unsold

class Narrowed a b | a -> b -- one glaze to a kiln
  -- | a class with no body
