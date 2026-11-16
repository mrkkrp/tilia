{-# LANGUAGE PatternSynonyms #-}

module Vector.Polar where

pattern (:|) :: Double -> Double -> Pair
pattern (:|) x y = x :& y

pattern (:%) :: Double -> Double -> Pair
pattern (:%) {(%<), (%>)} = (%<) :& (%>)
