{-# LANGUAGE TypeFamilies #-}

module Shape.Fallback where

import Data.Kind (Type)

type family Fallback (a :: k) :: k

type instance Fallback @Type a = Maybe a

type instance Fallback @(Type -> Type) f = f
