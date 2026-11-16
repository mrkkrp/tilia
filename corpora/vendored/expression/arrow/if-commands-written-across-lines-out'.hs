{-# LANGUAGE Arrows #-}

module Mixer.Gate where

open = proc level -> if loud then pass -< level else hold -< level

route = proc level ->
    if exceeds threshold level
        then
            compress
                . limit
                -<
                    level
        else
            pass
                -<
                    level
