module Data.Ring (add, scale) where

add :: (Num a) => a -> a -> a
add = (+)
{-# INLINABLE add #-}

scale :: (Num a) => a -> a -> a
scale = (*)
{-# INLINEABLE scale #-}
