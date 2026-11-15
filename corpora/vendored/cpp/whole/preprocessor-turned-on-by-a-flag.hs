{-# OPTIONS -cpp #-}
module Version where

#if __GLASGOW_HASKELL__ >= 900
f :: Int
#else
f :: Integer
#endif
f = 1
