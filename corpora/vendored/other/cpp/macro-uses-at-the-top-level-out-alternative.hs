{-# LANGUAGE CPP #-}

module Terminal.Named where

class Named a where
  name :: a -> String

#define NAMED(ty, str) \
instance Named ty where { name _ = str }

#define NAMED_PAIR(a, b) \
NAMED(a, "first") ; \
NAMED(b, "second")

NAMED(Int, "int")
NAMED(Bool, "bool")

NAMED_PAIR(Char, Double)
