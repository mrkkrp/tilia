{-# LANGUAGE Arrows #-}

module Mixer.Envelope where

import Control.Arrow (returnA)

attack = proc (level, rate) ->
    (\step -> ramp -< level + step * rate) 1

release = proc samples ->
    (| sustain (\s -> returnA -< s) |) samples

decay = proc (level, mode) ->
    ( case classify mode of
        Linear -> \t -> fade -< level - t
        Curved -> \t -> curve -< level * exp (negate t)
    )
        0.5

hold = proc (level, frames) ->
    (|
        repeatFor
            (sample -< level)
    |)
        frames
