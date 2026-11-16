{-# LANGUAGE Arrows #-}

module Mixer.Feedback where

echo = proc (input, gain) -> do
    rec output <- delay -< input + feedback
        feedback <-
            attenuate
                gain
                -<
                    output
    returnA -< output

loop = proc input -> do
    rec state <-
            accumulate
                state
                -<
                    input
    returnA -< state
