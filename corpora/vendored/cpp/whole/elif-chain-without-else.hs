{-# LANGUAGE CPP #-}

module M where

#if A
mid = 1
#elif B
mid = 2
#endif

after = 4
