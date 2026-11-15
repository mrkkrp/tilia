{-# LANGUAGE Arrows #-}

module Circuit.Gate where

import Control.Arrow ((<+>))

gate = proc signal -> do
  (trace -< signal) <+> (case signal of
    High -> count -< 1
    Low -> count -< 0)
  returnA -< signal
