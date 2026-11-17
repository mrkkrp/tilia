{-# LANGUAGE CPP #-}

module Shelf.Label where

class Shelved a where
#ifdef OLD_BASE
#ifdef WITH_NAMES
  name :: a -> String
#endif
#else
  label :: a -> String
#ifdef WITH_NAMES
  name :: a -> String
#endif
#endif
