{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Mixer.Route where

import Control.Arrow (returnA)

pan = proc (left, right) ->
    (\(l, r) -> balance -< l - r) (left * 2, right * 2)

route = proc track ->
    ( \case
        Vocal -> boost -< track
        _ -> returnA -< track
    )
        (kind track)

gate threshold = proc level ->
    if level > threshold
        then do
            opened <- open -< level
            returnA -< opened
        else do
            close -< ()
            returnA -< 0
