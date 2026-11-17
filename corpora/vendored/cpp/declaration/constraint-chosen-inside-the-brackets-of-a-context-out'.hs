{-# LANGUAGE CPP #-}

module Shop.Order where

sorted
    :: (
#if defined(STABLE)
         Ord a
#else
         Eq a
#endif
       )
    => [a] -> [a]
sorted = id

instance
    (
#if defined(STABLE)
      Ord a
#else
      Eq a
#endif
    )
    => Order [a]
    where
    order = id
