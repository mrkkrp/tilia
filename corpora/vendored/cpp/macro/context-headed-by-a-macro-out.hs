{-# LANGUAGE CPP #-}

-- A macro may stand for a whole constraint and its arrow, or for a pragma,
-- so a context headed by one gets no parentheses that would close it in.
module M where

import GHC.Stack (HasCallStack)

#define CALLSTACK HasCallStack =>
#define OVERLAPS {-# OVERLAPPING #-}

assertEqual :: CALLSTACK (Eq a, Show a) => a -> a -> IO ()
assertEqual expected actual = print (expected == actual)

firstOf ::
  CALLSTACK
    Foldable
    t =>
  t a -> a
firstOf = foldr const (error "empty")

same :: (Eq a) => a -> a -> Bool
same = (==)

class Pretty a where
  pretty :: a -> String

instance OVERLAPS (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = pretty a <> pretty b
