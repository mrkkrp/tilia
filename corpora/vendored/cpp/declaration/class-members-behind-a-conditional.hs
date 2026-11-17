{-# LANGUAGE CPP #-}

module Shelf.Label where

class Shelved a where
#ifdef WITH_LABELS
  label :: a -> String
#endif
