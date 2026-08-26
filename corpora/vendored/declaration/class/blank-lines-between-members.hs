{-# LANGUAGE DefaultSignatures #-}

class Storable a where
  put :: a -> Bytes

  -- for when there is no hand-written instance
  default put :: Generic a => a -> Bytes

  -- which is what this then uses
  put = genericPut

class Sized a where
  width :: a -> Int

  height :: a -> Int

class Packed a where
  pack :: a -> Bytes
  unpack :: Bytes -> a

type family Width a where
  -- the ones that fit in a machine word
  Width Int = 64
  Width Word = 64

  -- and the ones that do not
  Width Integer = Unbounded
  Width Rational = Unbounded
