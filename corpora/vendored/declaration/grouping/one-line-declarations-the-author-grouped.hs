{-# LANGUAGE TypeFamilies #-}

module Geometry.Units where

infixl 6 |+|
infixl 7 |*|
(|+|), (|*|) :: Length -> Length -> Length
(|+|) = lift2 (+)
(|*|) = lift2 (*)

type Metres = Double
type Seconds = Double
newtype Length = Length Metres
newtype Duration = Duration Seconds

class Measured a
instance Measured Length
instance Measured Duration

type family Unit a
type instance Unit Length = Metres
-- A note written against the run it stands in, and staying inside it.
type instance Unit Duration = Seconds

zero = Length 0
unit = Length 1
lift2 f (Length a) (Length b) =
  Length (f a b)
double = lift2 (+) unit unit
-- | Set apart by its Haddock.
half = Length 0.5
quarter = Length 0.25
