{-# LANGUAGE TupleSections #-}
{-# LANGUAGE
    ScopedTypeVariables
  , LambdaCase -- for classify
  , {- and for nothing else -} BangPatterns
  #-}
{-# LANGUAGE DataKinds #-}
module Kiln.Commented where

-- GHC skips the comments inside the pragma and reads the names around
-- them, and a comment there stays where it is written.
classify :: Int -> String
classify = \case
  0 -> "zero"
  _ -> "other"
