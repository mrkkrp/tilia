{-# LANGUAGE TypeFamilyDependencies #-}

module Wire.Frame where

import Data.Kind (Type)

type family Envelope payload = (envelope :: Type) | envelope -> payload
