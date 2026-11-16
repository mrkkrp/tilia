{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}

module Codec.Size where

import Data.Kind (Type)

sizeOf :: forall (a :: Type) -> (Storable a) => Int
sizeOf (type (a :: Type)) = byteSize @a

unwrapped :: Wrapped (Maybe Int) -> Proxy Int
unwrapped (Wrapped @(f a)) = Proxy @a
