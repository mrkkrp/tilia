f :: (Ord a) => a -> a
f = f

{-# SPECIALISE let x = 2 in f x #-}
