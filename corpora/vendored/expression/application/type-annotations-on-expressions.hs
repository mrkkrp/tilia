{-# LANGUAGE TypeAbstractions #-}

module Pair.Literal where

origin :: (Int, Double)
origin = (0 :: Int, 0.5 :: Double)

swapped = (\ @a x @b y -> (y :: b, x :: a) )
    :: forall a. a -> forall b. b -> (b, a)

limit = (maxBound :: Int) `div` 2
