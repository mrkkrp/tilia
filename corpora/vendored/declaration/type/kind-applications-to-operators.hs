module Registry.Equal where

import Data.Kind (Type)
import Data.Type.Equality ((:~~:))

data (a :: k) :=: (b :: k) = Equal

type SameType = (:=:) @Type

type Matching = (:~~:) @(Type -> Type) @Type
