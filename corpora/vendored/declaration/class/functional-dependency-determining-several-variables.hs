{-# LANGUAGE FunctionalDependencies #-}

module Index.Lookup where

class Indexed store key value | store -> key value where
  lookupIn :: store -> key -> Maybe value
