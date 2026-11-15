{-# LANGUAGE RequiredTypeArguments #-}

module Memory.Layout where

sizeOf :: forall a -> (Storable a) => Int
sizeOf = undefined

alignmentOf
  :: forall a
  -> Storable a
  => Int
alignmentOf = undefined
