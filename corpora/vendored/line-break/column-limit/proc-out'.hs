{-# LANGUAGE Arrows #-}

-- A proc whose pattern runs past column 80 is broken before the pattern,
-- and its -> ends the pattern's line.
module Circuit.Switch where

switch threshold = proc
    (levelOfTheSignal, thresholdOfTheAlarm, rangeOfTheSensor) ->
    alarm -< levelOfTheSignal
