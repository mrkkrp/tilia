{-# LANGUAGE TypeFamilies #-}

module Plugin.Slot where

import Data.Kind (Type)

data Slot
    = forall
        ( f
            :: Type -> Type
        )
        (a :: Type)
       . Slot (f a)

data Handle where
    Handle
        :: forall
            ( m
                :: Type -> Type
            )
            (r :: Type)
         . m r -> Handle

connect
    :: forall
        ( m
            :: Type -> Type
        )
        (r :: Type)
     . m r -> ()
connect _ = ()

type family Apply f x y where
    forall
        ( f
            :: Type -> Type -> Type
        )
        (x :: Type)
        (y :: Type)
     . Apply f x y =
        f x y
