{-# LANGUAGE CPP #-}

module Data.UniqMap where

-- | A map keyed by the unique of each key.
data UniqMap a
  =
#if MIN_VERSION_containers(0,6,0)
  UniqMap{unUniqMap :: IntMap a}
#else
  UniqMap{unUniqMap :: Map Int a}
#endif
