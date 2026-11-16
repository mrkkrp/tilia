module Pottery.Wheel where

type Spin =
  -- | How fast the wheel turns.
  Int ->
  -- the clay is centred first
  Clay
