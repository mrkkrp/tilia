{-# LANGUAGE CPP #-}

module M where

before = 1

#ifdef FOO
mid = case x of
  A -> 1
  B -> 2
#else
mid = 3
#endif

after = 4
