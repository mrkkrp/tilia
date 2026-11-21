{-# LANGUAGE CPP #-}

module Data.Propagator.Cell where

#define M IO

-- | What a propagator does when its cell changes.
data Result
  = StillBottom (M ()) -- ^ Wait for another update.
  | Done -- ^ Nothing is left to do.
