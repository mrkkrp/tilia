module Codec.Checksum where

checksum :: [Int] -> Int
checksum = foldr (+) 0
{-# OPAQUE checksum #-}

mask :: Int -> Int
mask = (`mod` 256)
{-# INLINE CONLIKE mask #-}

seed :: Int
seed = 7
{-# NOINLINE   CONLIKE [1] seed #-}
