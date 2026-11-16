{-# LANGUAGE LambdaCase #-}

module Route.Lane where

pickLane :: Lane
pickLane = \cases | busy -> Freight | quiet -> Mixed | otherwise -> Empty
