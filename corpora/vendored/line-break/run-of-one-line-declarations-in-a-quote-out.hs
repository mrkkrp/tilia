{-# LANGUAGE TemplateHaskell #-}

-- A run of one-line declarations in a declaration quote keeps its lines as
-- well, wherever the quote puts them.
module Loom.Quoted where

declarations =
  [d|
    tension = 3
    weave threads thread colour = summarise (findWithDefault colour threads) xyyyy
    shuttleCount = 2
    |]
