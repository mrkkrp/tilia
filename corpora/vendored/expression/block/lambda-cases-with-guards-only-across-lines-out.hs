{-# LANGUAGE LambdaCase #-}

module Route.Lane where

pickLane :: Lane
pickLane = \cases
  | busy -> Freight
    | quiet -> Mixed
    | otherwise -> Empty

pickSide :: Side
pickSide = \cases
  | crowded -> Left
  | otherwise -> Right
