{-# LANGUAGE TypeAbstractions #-}

module Shelf.Tag where

import Data.Kind (Type)

data Tagged @k (t :: k) a = Tagged a

type Ignore :: Type -> Type -> Type
type Ignore a _ = a

untag :: forall {k} (t :: k) a. Tagged t a -> a
untag (Tagged x) = x
