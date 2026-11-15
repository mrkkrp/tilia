{-# LANGUAGE CPP #-}

module Terminal.Width where

base :: Int
base  =  80

width :: Int
width  =
  base
#ifdef WITH_MARGIN
    + 4
#endif

height :: Int
height  =  24
