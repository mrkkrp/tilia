f :: (Ord a) => a -> a
f = f

{-# SPECIALIZE let x = 2 in f x #-}
