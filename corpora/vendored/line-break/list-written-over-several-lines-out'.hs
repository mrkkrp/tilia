-- The items of a list written over several lines keep their lines, so that
-- the rows of a table stay rows.
module Loom.Weaves where

weaves =
  [ ("plain", Weave Plain 1 1, "one thread over and one under, the simplest weave of all")
  , ("twill", Weave Twill 2 1, "diagonal ribs")
  ]
