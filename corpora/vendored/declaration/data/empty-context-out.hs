{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GADTs #-}

module Report.Cells where

data () => Cell a = Cell a

newtype () => Width = Width Int

data Some = forall a. () => Some a

data Shown where
  Shown :: () => Int -> Shown
