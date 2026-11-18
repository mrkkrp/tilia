{-# LANGUAGE CPP #-}

module Shelf.Crate where

#ifdef WITH_LABELS
type Crate item label =
#else
type Crate item =
#endif
  [item]

#ifdef WITH_LABELS
type Box item label
#else
type Box item
#endif
  = Maybe item
