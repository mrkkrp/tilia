{-# LANGUAGE CPP #-}

module M where

#ifdef FOO
f = (1
#endif
  + 2)
