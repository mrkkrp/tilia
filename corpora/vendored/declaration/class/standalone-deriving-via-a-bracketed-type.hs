{-# LANGUAGE DerivingVia #-}

module Score.Total where

import Data.Monoid (Sum (..))

newtype Score = Score Int

deriving via (Sum Int) instance Semigroup Score
