{-# LANGUAGE RequiredTypeArguments #-}

module Codec.Shape where

describe :: forall t -> Typeable t => String
describe t = show (typeRep @t)

showable :: String
showable = describe (forall a. (Show a, Eq a) => a -> String)

constrained :: String
constrained = describe (Ord [Int] => Int)
