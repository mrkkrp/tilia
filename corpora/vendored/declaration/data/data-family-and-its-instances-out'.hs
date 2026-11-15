{-# LANGUAGE TypeFamilies #-}

module Sensor.Buffer where

data family Buffer a

data instance Buffer Int = IntBuffer [Int]

newtype instance Buffer Bool = BoolBuffer Integer

data instance Buffer Double
    = Empty
    | Samples [Double] Int
