module Kiln.Firing where

heat, cool :: Int -> Int
heat = (+ 10)
cool = subtract 10
{-# DEPRECATED heat "Use raise instead."; cool "Use lower instead." #-}

{-# WARNING
  in "x-unsafe" vent "Opens the door hot.";
  in "x-partial" peak "Fails on an empty schedule."
  #-}
vent :: Int -> Int
vent = const 0

peak :: [Int] -> Int
peak = maximum
