module Codec.Fast where

encode :: String -> String
encode = id
{-# INLINE [1] encode #-}

decode :: String -> String
decode = id
{-# NOINLINE [~1] decode #-}

compose :: Int -> Int
compose = id
{-# INLINE [2] compose #-}

{-# SPECIALISE [1] scale :: Int -> Int #-}
scale :: Num a => a -> a
scale = (* 2)
