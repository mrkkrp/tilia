{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Codec.Wrapped where

instance forall a. (Show a) => Show (Wrapped a) where
    show (Wrapped x) = "Wrapped " <> show x

deriving instance forall a. (Eq a) => Eq (Wrapped a)

newtype Count = Count Int
    deriving (Semigroup) via forall k. Const (Sum Int) (k :: Type)
