{-# LANGUAGE GADTs #-}

-- A documented field of a record written on one line is broken where it
-- runs past column 80, wherever its Haddock goes.
module Kiln.Batch where

data Batch where
    Batch
        :: { firedOn
                :: Map
                    KilnIdentifier
                    (Either FiringFailure (NonEmpty FiringRecord))
             -- ^ When it was fired.
           }
        -> Batch

data Order = Order
    { firedOn
        :: Map KilnIdentifier (Either FiringFailure (NonEmpty FiringRecord))
      -- ^ When it was fired.
    }
