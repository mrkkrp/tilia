{-# LANGUAGE Arrows #-}

module Mixer.Shape where

soften = proc level -> (smooth . clip) -< level

shape = proc level ->
  ( (boost)
      (curve)
  )
    -<
      ( scale
          level
      )

limit = proc signal -> do
  ( proc s ->
      ceiling -< s
    )
    -<
      signal
