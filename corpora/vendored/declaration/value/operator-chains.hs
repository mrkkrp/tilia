module M where

atTheLoosest =
  a * b
    + c * d

capturedLocally =
  a * b
    + c * d
  where
    infixl 8 +
    x + y = x

blockHandedOn = g $ do
  h
  i
