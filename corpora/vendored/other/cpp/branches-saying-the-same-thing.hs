{-# LANGUAGE CPP #-}

module M where

#ifdef FOO
mid = 2
#else
mid  =  2
#endif
