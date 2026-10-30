module Data.Ring (add, scale, Ring (..)) where

add :: (Num a) => a -> a -> a
add = (+)
{-# SPECIALISE add :: Int -> Int -> Int #-}
{-# SPECIALIZE add :: Double -> Double -> Double #-}

scale :: (Num a) => a -> a -> a
scale = (*)
{-# SPECIALISE INLINE scale :: Int -> Int -> Int #-}
{-# SPECIALIZE NOINLINE scale :: Double -> Double -> Double #-}

newtype Ring a = Ring a

instance (Eq a) => Eq (Ring a) where
  {-# SPECIALISE instance Eq (Ring Int) #-}
  Ring a == Ring b = a == b

instance (Ord a) => Ord (Ring a) where
  {-# SPECIALIZE instance Ord (Ring Int) #-}
  compare (Ring a) (Ring b) = compare a b
