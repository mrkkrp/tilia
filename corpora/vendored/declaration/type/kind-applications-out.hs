module Schema.Field where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))

type Untyped = Proxy @Type

type Wrapper = Proxy @(Type -> Type) Maybe

fieldProxy :: Proxy @Type Int
fieldProxy = Proxy
