{-# LANGUAGE CPP #-}

module Data.Propagator.Cell where

#define M IO

-- | What a propagator does when its cell changes.
data Result
  = -- | Wait for another update.
    StillBottom (M ())
  | -- | Nothing is left to do.
    Done
