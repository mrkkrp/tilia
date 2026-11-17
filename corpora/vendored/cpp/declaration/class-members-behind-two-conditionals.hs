{-# LANGUAGE CPP #-}

module Shelf.Label where

class Shelved a where
#ifdef OLD_BASE
#else
  label :: a -> String
#endif
#ifdef WITH_NAMES
  name :: a -> String
#endif
