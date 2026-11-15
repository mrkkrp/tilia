{-# LANGUAGE CPP #-}

module M where

-- above
before = 1 -- trailing

#ifdef FOO
mid = 2
#else
mid = 3
#endif

-- below
after = 4
