{-# OPTIONS_GHC -XNoLambdaCase #-}
{-# LANGUAGE LambdaCase #-}

module Kiln.Case where

-- GHC reads the flags of the options pragmas and the names of the language
-- pragmas in the order they are written, so LambdaCase, named last, is on.
firing :: Int -> Int
firing = \case
  0 -> 1
  n -> n
