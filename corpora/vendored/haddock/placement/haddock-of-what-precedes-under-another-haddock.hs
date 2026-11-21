module Derived.Types where

data Tree = Leaf | Node Tree Tree
-- TODO: derive the arity once families are handled
-- $(deriving_th ''Tree)

-- ^ A recursive definition.
data List a = Nil | Cons a (List a)

size :: Int
size = 1
-- ^ The size.

-- ^ More on the size.
