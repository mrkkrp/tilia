{-# LANGUAGE CPP #-}

module Kiln.Glaze where

-- An operator written under a conditional that chooses its left operand
-- begins its line, rather than end the operand's.
glaze :: IO [Int]
glaze = do
  c <- pure 1
#if MIN_VERSION_base(4,0,0)
  fire c 1
#else
  fire c 2
#endif
    $ cool c
    : []
