module Plugin.Entry where

triple :: Int -> Int
triple = (* 3)

foreign export ccall triple :: Int -> Int

foreign export ccall "plugin_scale" scale :: Double -> Double -> Double

scale :: Double -> Double -> Double
scale = (*)
