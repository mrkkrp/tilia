-- A binding with a comment lined up under it keeps its line however long,
-- so that the comment stays lined up.
module Loom.Comments where

bobbins threads = do
  let tightened = map (adjustTension tension) (spinAll threads (length threads) defaultTwist)
      -- The comment that holds the binding on its line.
  pure tightened
