{-# LANGUAGE CPP #-}

module M where

f =
  a
#if X
    + b
#endif
#if Y
    + c
#endif
