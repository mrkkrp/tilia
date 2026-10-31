{-# LANGUAGE Arrows #-}

module Circuit.Gate where

gate = proc signal -> do
  (case signal of
     High -> open -< signal
     Low -> shut -< signal)
  (case signal of
     High -> count -< 1
     Low -> count -< 0) <+> (trace -< signal)
  returnA -< signal
