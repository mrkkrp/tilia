module Signal.Clamp where

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi
{-# SPECIALISE forall lo. clamp (lo :: Int) #-}
{-# SPECIALISE [1] forall a. forall (hi :: [a]). clamp [] hi #-}
