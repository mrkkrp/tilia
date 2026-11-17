{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Codec.Width where

import Foreign.Storable (Storable, sizeOf)

widthOf :: forall a -> Storable a => Int
widthOf (type a) = sizeOf (undefined :: a)

header :: Int
header = widthOf (type Word32) + widthOf (type (Ptr Word8))
