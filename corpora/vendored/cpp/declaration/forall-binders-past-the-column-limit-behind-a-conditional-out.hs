{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}

module Kiln.Clock where

import Data.Kind (Type)

-- The binders of a forall that a conditional varies whole stay in each
-- branch on one line, past column 80 as well.
lookupCont ::
#if defined(__GLASGOW_HASKELL__)
  forall (result :: Type) (key :: Type) (value :: Type) (container :: Type -> Type).
#else
  forall (key :: Type) (value :: Type) (container :: Type -> Type) (result :: Type).
#endif
  key -> (value -> result) -> container value -> result
lookupCont = undefined
