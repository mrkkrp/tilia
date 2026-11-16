{-# LANGUAGE TypeAbstractions #-}

module Shelf.Keep where

import Data.Kind (Type)

type Keep :: Type -> Type -> Type
type Keep a (_ :: Type) = a
