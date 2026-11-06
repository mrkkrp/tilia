{-# LANGUAGE CPP #-}

-- | Things to export, or none.
module Things
#ifdef HIDE
  () where
#else
  ( thing,
    other,
  )
where

thing :: Int
thing = 1

other :: Int
other = 2
#endif
