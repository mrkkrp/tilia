module Functor.Nest where

import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))

type Nested = Compose @Type @Type Maybe []

type Wrapped = Proxy @(Type -> Type) (Maybe :: Type -> Type)
