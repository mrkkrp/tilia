{-# LANGUAGE CPP #-}
#ifdef USE_MAGIC_PROXY
{-# LANGUAGE MagicHash #-}
#endif

module Data.Map.Internal where

import Data.Maybe (fromMaybe)
#if __GLASGOW_HASKELL__
import GHC.Exts (build, lazy)
#  ifdef USE_MAGIC_PROXY
import GHC.Exts (Proxy#, proxy#)
#  endif
import Data.Coerce
#endif
