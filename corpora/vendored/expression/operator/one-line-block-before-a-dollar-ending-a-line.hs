module Kiln.Firing where

adjust :: Maybe (Int -> Int) -> Int -> Int
adjust override heat =
  case override of { Nothing -> id; Just f -> f } $
    max 600 heat
