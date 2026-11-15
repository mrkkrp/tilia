{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module Pair.Both where

data a :* b = a :* b

infixr 6 data :*

{-# DEPRECATED data (:*) "Use the Both constructor instead." #-}
{-# WARNING type (:*) "The type is going away." #-}
