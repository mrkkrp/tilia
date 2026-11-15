-- An operand of a command chain other than the last stays on one line as
-- written where it runs past column 80, as in a chain of expressions.
{-# LANGUAGE Arrows #-}

module Circuit.Watch where

import Control.Arrow (returnA, (<+>))

watch = proc x ->
    (record -< describeTheSignal (signalStrength x) (signalPhase x) (sampleRate x)) <+> do
        y <- sense -< x
        returnA -< y
