{-# LANGUAGE Arrows #-}

module Mixer.Channel where

blend = proc (left, right) -> average -< (left, right)

pan = proc
  ( level,
    position
  ) -> mix -< (level * position, level * (1 - position))

mute = proc _ -> returnA -< silence

start = proc () -> do
  clock -< ()
  returnA -< silence

route = proc (Stereo left right) -> merge -< (left, right)
