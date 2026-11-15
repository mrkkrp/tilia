module Pricing.Rounding where

roundDown :: Double -> Int
roundDown =
  {- toward zero -} truncate
