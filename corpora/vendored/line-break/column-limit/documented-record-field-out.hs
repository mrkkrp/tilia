{-# LANGUAGE GADTs #-}

-- A documented field of a record written on one line is broken where it
-- runs past column 80, wherever its Haddock goes.
module Kiln.Batch where

data Batch where
  Batch ::
    { {- | When it was fired. -}
      firedOn ::
        Map KilnIdentifier (Either FiringFailure (NonEmpty FiringRecord))
    } ->
    Batch

data Order = Order
  { {- | When it was fired. -}
    firedOn ::
      Map KilnIdentifier (Either FiringFailure (NonEmpty FiringRecord))
  }
