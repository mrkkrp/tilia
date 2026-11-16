{-# LANGUAGE TypeFamilies #-}

-- A fixity declared for an operator that names a type and a constructor
-- applies to both, however the constructor is declared, so each operator
-- below nests under the one before it.
module Circuit.Wiring where

infixr 5 :+
infixr 6 :-
infixr 7 :*
infixr 8 :/

data a :+ b = a :+ b

data a :- b where
  (:-) :: a -> b -> a :- b

data family a :* b

data instance Int :* b = Int :* b

class Terminal t where
  data t :/ u

instance Terminal Bool where
  data Bool :/ u = Bool :/ u

type Wiring = Int
  :+ Char
  :- Int
  :* Bool
  :/ String

wiring :: Wiring
wiring = 1
  :+ 'a'
  :- 2
  :* True
  :/ "end"
