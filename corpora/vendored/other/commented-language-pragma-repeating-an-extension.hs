{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase -- for classify
  #-}
module Kiln.Repeated where

-- A pragma that names an extension again but holds a comment stays, with
-- the comment.
classify :: Int -> String
classify = \case
  0 -> "zero"
  _ -> "other"
