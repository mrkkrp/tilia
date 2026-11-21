module Kiln.Batch where

data Batch = Batch
    { pieces :: Int

      -- counted before the firing

      -- ^ How many pieces go in.
      -- Broken ones are left out.
    }
