{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}

module Resource.Lease where

data Lease m a where
  Lease :: a %m -> Lease m a
