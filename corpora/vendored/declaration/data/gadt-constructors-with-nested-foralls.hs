{-# LANGUAGE GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Label.Tagged where

import Data.Kind (Type)

type Tagged :: k -> Type
data Tagged t where
  Tag :: forall k. forall (t :: k). Ord k => k -> Tagged t
  Named :: forall k. forall (t :: k) -> String -> Tagged t
