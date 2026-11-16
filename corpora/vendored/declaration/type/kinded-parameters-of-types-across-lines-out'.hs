{-# LANGUAGE TypeFamilies #-}

module Ledger.Shape where

import Data.Kind (Type)

type Cell
    ( a
        :: Type
    ) =
    Maybe a

data
    Column
        ( f
            :: Type -> Type
        ) = Column (f Int)

type family
    Width
        ( s
            :: Type
        )
        :: Type
