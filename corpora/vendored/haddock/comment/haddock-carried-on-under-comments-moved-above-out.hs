module Kiln.Batch where

data Batch = Batch
  { -- | How many pieces go in.
    -- Broken ones are left out.
    pieces :: Int
    -- counted before the firing
  }
