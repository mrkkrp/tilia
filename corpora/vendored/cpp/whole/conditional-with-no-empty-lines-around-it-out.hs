{-# LANGUAGE CPP #-}

module M where

before = 1

#ifdef FOO
mid = 2
#else
mid = 3
#endif

after = 4
