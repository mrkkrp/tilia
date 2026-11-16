{-# LANGUAGE TypeFamilyDependencies #-}

module Wire.Codec where

type family Encoded a = bytes | bytes -> a

type family Framed header body = frame | frame -> header body

type family Tagged label = tagged where
    Tagged label = (label, Int)

type family Packed a b c = packed | packed -> a b c where
    Packed a b c = (a, b, c)
