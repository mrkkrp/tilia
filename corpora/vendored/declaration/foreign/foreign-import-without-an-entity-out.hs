module Math.Libm where

foreign import ccall sin :: Double -> Double

foreign import ccall unsafe cos :: Double -> Double
