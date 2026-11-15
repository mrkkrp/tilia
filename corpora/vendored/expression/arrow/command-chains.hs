{-# LANGUAGE Arrows #-}

module Circuit.Merge where

import Control.Arrow ((<+>), (|||))

anyOf = proc x -> (sense -< x) <+> (probe -< x) <+> (guess -< x)

either' = proc x -> (left -< x) <+> (right -< x) ||| (fallback -< x)

routed mode = proc x -> (log -< x) <+> case mode of
  Fast -> quick -< x
  Slow -> careful -< x

guarded = proc x -> (attempt -< x) `handleA` \err ->
  recover -< (x, err)
