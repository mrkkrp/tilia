{-# LANGUAGE PatternSynonyms #-}

-- A fixity declared for an operator that names a type and a record field
-- applies to the field too, whether a newtype, a GADT or a pattern synonym
-- declares it, so each operator below nests under the one before it.
module Arrow.Apply where

infixr 1 ~>
infixr 2 <~
infixr 3 |>

newtype a ~> b = Morph {(~>) :: a -> b}

type a <~ b = b ~> a

pattern Flipped :: (b -> a) -> a <~ b
pattern Flipped {(<~)} = Morph (<~)

data a |> b where
  Pipe :: {(|>) :: a -> b} -> a |> b

applied :: (Int ~> Int) -> (Int <~ Int) -> (Int |> Int) -> Int -> Int
applied f g h x = f
  ~> g
  <~ h
  |> x + 1
