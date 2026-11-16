{-# LANGUAGE PatternSynonyms #-}

-- A fixity declared for an operator that names a type and a term applies
-- to the term too, be it a method, a pattern synonym or a foreign import,
-- so each operator below nests under the one before it.
module Interval.Bounds where

infixr 1 :<
infixr 2 \/
infixr 3 /\

type a :< b = (a, b)

type a \/ b = Either a b

type a /\ b = (a, b)

pattern (:<) :: a -> b -> a :< b
pattern lower :< upper = (lower, upper)

class Lattice l where
  (\/) :: l -> l -> l

instance Lattice Double where
  (\/) = max

foreign import ccall unsafe "math.h fmin" (/\) :: Double -> Double -> Double

bounds :: Double -> Double -> Double -> Double :< Double
bounds x y z =
  0
    :< x
      \/ y
        /\ z + 1
