{-# LANGUAGE CPP #-}

module B1 (throw) where

#ifdef X
import Control.Exception hiding (throw)

throw :: (Exception e) => e -> a
throw = undefined
#else
import Control.Exception
#endif
