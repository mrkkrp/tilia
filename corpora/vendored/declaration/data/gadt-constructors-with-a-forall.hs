{-# LANGUAGE GADTs #-}

module Event.Payload where

data Payload where
  Payload :: forall a. Show a => a -> Payload
  Pair ::
    forall a b.
    (Show a, Show b) =>
    a ->
    b ->
    Payload
  Labelled :: forall a. { label :: String, value :: a } -> Payload
