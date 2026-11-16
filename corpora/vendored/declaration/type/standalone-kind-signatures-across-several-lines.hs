module Pipeline.Stage where

import Data.Kind (Constraint, Type)

type Stage :: (Type -> Type)
           -> Type
           -> Type
data Stage m a = Stage (m a)

type Runnable ::
  (Type -> Type) -> Constraint
class Runnable m

type Label :: Type
type Label = String
