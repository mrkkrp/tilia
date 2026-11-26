{-# LANGUAGE CPP #-}

module Shelf.Label where

class Shelved a
#ifdef WITH_LABELS
  where
  label :: a -> String
#endif

class Named a
#ifdef WITH_LABELS
  where
  label :: a -> String
#else
  where
  name :: a -> String
#endif
