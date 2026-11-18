{-# LANGUAGE CPP #-}

module Shelf.Crate where

type
#ifdef WITH_LABELS
  Crate item label =
#else
  Crate item =
#endif
  [item]

type
#ifdef WITH_LABELS
  Box item label =
#else
  Box item =
#endif
  Maybe item
