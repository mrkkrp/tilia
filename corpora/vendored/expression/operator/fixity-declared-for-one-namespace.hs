{-# LANGUAGE ExplicitNamespaces #-}

-- A fixity declared with a namespace applies to the type or to the
-- constructor of its name alone: :* binds tighter than :+ in Shape, and
-- looser in shape.
module Shape.Sum where

infixr 6 type :+
infixr 7 type :*
infixr 7 data :+
infixr 6 data :*

data a :+ b = a :+ b

data a :* b = a :* b

type Shape = Int
  :+ Int
  :* Int

shape :: Int :* (Int :+ Int)
shape = 1
  :* 2
  :+ 3
