{-# LANGUAGE CPP #-}

module Terminal.Derived where

class Described a

#define describe(ty)      \
instance Described ty where { \
  }                           \

describe(Int)
describe(Bool)
