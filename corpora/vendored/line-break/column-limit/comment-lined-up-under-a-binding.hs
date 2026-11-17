-- A binding that runs past column 80 with a comment lined up under it is
-- broken, and the comment stays lined up with the start of the binding.
module Loom.Comments where

bobbins threads = do
  let tightened = map (adjustTension tension) (spinAll threads (length threads) defaultTwist)
      -- The comment lined up with the binding.
  pure tightened
