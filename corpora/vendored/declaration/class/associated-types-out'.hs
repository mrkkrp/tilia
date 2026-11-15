{-# LANGUAGE TypeFamilies #-}

module Store.Container where

import Data.Kind (Type)

class Container f where
    type Elem f :: Type
    type Elem f = Int
    data Cursor f
    empty :: f
    insert :: Elem f -> f -> f

instance Container [a] where
    type Elem [a] = a
    data Cursor [a] = ListCursor Int
    empty = []
    insert = (:)
