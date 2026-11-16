{-# LANGUAGE GADTs #-}

module Query.Expr where

import Data.Type.Equality ((:~:))

data Expr a where
    Literal :: Int -> Expr Int
    Lambda :: (Expr a -> Expr b) -> Expr (a -> b)
    Cast :: a :~: b -> Expr a -> Expr b
    Pair
        :: (Expr a, Expr b)
        -> a :~: b
        -> Expr (a, b)
