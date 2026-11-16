{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeOperators #-}

module Circuit.Wire where

import Data.Kind (Type)

type (:+:) :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data (:+:) f g a = Wired (f a) | Bridged (g a)

type role (:+:) representational representational nominal

type (~>) f g = forall a. f a -> g a
