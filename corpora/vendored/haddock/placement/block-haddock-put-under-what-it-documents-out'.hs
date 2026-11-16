{-# LANGUAGE GADTs #-}

module Pottery.Order where

data Order = Order
    { count :: Int
      -- ^ How many pots.
    }

data Batch where
    Batch
        :: { firedOn :: Day
             -- ^ When it was fired.
           }
        -> Batch
    Sample
        :: Int
        -- ^ How many to try.
        -> Batch
    Seconds
        :: Int
        -> Batch
        -- ^ What is left.
