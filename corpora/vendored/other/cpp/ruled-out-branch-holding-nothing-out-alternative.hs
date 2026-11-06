{-# LANGUAGE CPP #-}

module M where

#if FLAG
g = 1
#else
#if FLAG
#else
f = 1
#endif
#endif
