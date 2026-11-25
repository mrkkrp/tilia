{-# LANGUAGE NPlusKPatterns #-}

module Peano.Steps where

predecessor :: Int -> Int
predecessor
  ( n
      + 1
  ) = n

lastStep :: Int
(lastStep + 1) = 10
