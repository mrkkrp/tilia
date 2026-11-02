{-# LANGUAGE CPP #-}

module Budget where

rent = 900
food = 300
#if defined(CAR)
  + 120
#endif
savings = 200
