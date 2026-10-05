{-# LANGUAGE CPP #-}

module B1 (throw) where

import Control.Exception
#ifdef X
  hiding (throw)

throw :: (Exception e) => e -> a
throw = undefined
#endif
