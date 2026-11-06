{-# LANGUAGE Arrows #-}

module Cache.Pipeline where

widen f = proc entry -> do -- one stage at a time, so a failure names the stage
  warmed <- f -< entry
  returnA -< warmed
