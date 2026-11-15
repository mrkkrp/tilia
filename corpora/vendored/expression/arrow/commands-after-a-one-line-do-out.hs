{-# LANGUAGE Arrows #-}

module Circuit.Fallback where

import Control.Arrow ((<+>))

guarded = proc x ->
  do sense -< x
    <+> (fallback -< x)
