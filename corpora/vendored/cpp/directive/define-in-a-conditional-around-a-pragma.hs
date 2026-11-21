{-# LANGUAGE CPP #-}
-- | Describing values, more specifically where the compiler lets an
-- instance say that it overlaps.

#if __GLASGOW_HASKELL__ >= 710
#define OVERLAPPING_ {-# OVERLAPPING #-}
#else
{-# LANGUAGE OverlappingInstances #-}
#define OVERLAPPING_
#endif

module Data.Describe where

class Describe a where
  describe :: a -> String

instance Describe a => Describe [a] where
  describe = concatMap describe

instance OVERLAPPING_ Describe [Char] where
  describe = id
