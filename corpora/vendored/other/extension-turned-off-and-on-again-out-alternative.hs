{-# LANGUAGE NoLambdaCase #-}
{-# LANGUAGE LambdaCase #-}

module Kiln.Again where

-- The name written last decides, so LambdaCase is on.
firing :: Int -> Int
firing = \case
  0 -> 1
  n -> n
