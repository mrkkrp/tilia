{-# LANGUAGE FunctionalDependencies #-}

module Unit.Convert where

class Convert from to result | from to -> result, result -> from where
  convert :: from -> to -> result

class Scale a b | a -> b, b -> a where
  scale :: a -> b
