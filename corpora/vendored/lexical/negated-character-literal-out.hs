module Cipher.Mirror where

instance Num Char where
  negate c = toEnum (255 - fromEnum c)

mirroredA :: Char
mirroredA = -'a'
