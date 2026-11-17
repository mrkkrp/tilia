module Stats.Mean where

mean :: Fractional a => [a] -> a
mean xs = sum xs / fromIntegral (length xs)
{-# SPECIALISE mean :: [Double] -> Double, [Float] -> Float #-}
