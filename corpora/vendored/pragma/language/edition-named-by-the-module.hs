{-# LANGUAGE GHC2024 #-}
module Kiln.Edition where

-- GHC compiles a module under the edition it names, so LambdaCase, which
-- GHC2024 turns on, is on.
firing :: Int -> Int
firing = \case
  0 -> 1
  n -> n
