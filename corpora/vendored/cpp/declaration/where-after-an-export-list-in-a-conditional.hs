{-# LANGUAGE CPP #-}
-- | Shapes, with an export list unless the doctests read the module.
module Shapes
#ifndef DOCTEST
  ( Shape (..)
  , area
  )
#endif
where

data Shape = Square Double | Circle Double

area :: Shape -> Double
area (Square a) = a * a
area (Circle r) = pi * r * r
