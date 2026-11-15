{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Geometry.Point where

data Point = Point Int Int

deriving stock instance Eq Point

deriving stock instance Show Point

newtype Distance = Distance Double

deriving newtype instance Num Distance

deriving anyclass instance Describable Point
