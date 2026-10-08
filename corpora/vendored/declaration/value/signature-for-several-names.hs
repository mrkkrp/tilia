{-# LANGUAGE PatternSynonyms #-}

module Gears.Shift where

shiftUp, shiftDown, skipUp, skipDown
  :: forall n . Int -> n -> Int -> n
shiftUp = undefined
shiftDown = undefined
skipUp = undefined
skipDown = undefined

lowGear,
  highGear
  :: Int
lowGear = 1
highGear = 5

class Gearbox g where
  engage, release
    :: g -> g

pattern Neutral, Reverse
  :: Int
pattern Neutral = 0
pattern Reverse = -1
