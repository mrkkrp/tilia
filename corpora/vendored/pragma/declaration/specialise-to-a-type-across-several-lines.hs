module Matrix.Scale where

scaleBy :: (Num a) => a -> [a] -> [a]
scaleBy k = map (* k)
{-# SPECIALISE scaleBy
      :: Int
      -> [Int]
      -> [Int] #-}
{-# SPECIALISE [~1] scaleBy :: Double ->
                                 [Double] -> [Double] #-}
