{-# LANGUAGE CPP #-}

module Completion.Names (fromName) where

import GHC

fromName :: RdrName -> HsExpr GhcPs
fromName name =
#if __GLASGOW_HASKELL__ >= 806
  HsVar
    NoExt
#else
  HsVar
#endif
    (L noSrcSpan name)
