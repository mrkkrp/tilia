{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tally.Count where

import Data.Monoid (Sum (..))

-- A comment written before or after a keyword of a deriving clause stays
-- there.
newtype Count = Count Int
  deriving Semigroup
    -- by addition
    via Sum Int
  deriving
    -- as numbers are
    stock (Eq)

newtype Score = Score Int
  deriving Semigroup
    via -- by addition
      ( -- of points
        Sum Int
      )

newtype Total = Total Int

-- So does one before or after a keyword of a standalone deriving
-- declaration.
deriving
  -- by addition
  via Sum Int
  instance Semigroup Total

deriving stock
  -- as numbers are
  instance Eq Total

deriving stock -- as numbers are
  instance -- and only so
  Ord Total
