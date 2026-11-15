{-# LANGUAGE CPP #-}

module Codec.Width where

width :: Codec -> Int
#if MIN_VERSION_base(4,18,0)
width (Codec _ size)
#else
width (Codec size)
#endif
  = size * 8
