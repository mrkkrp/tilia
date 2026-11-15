module Stats.Scale where

scale :: (Num a) => a -> [a] -> [a]
scale k = map (* k)

{-# SPECIALISE scale @Int #-}

{-# SPECIALISE scale (2 :: Double) #-}

total :: Int
total = 0
