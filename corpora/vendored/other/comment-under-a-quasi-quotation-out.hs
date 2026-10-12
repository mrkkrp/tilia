{-# LANGUAGE QuasiQuotes #-}

-- A comment under a quasi-quotation written over several lines stays out of
-- it.
module Physics.Body (setAngle, reindexed) where

setAngle :: Ptr Body -> Double -> IO ()
setAngle bodyPtr angle = [C.block| void {
  cpBodySetAngle($(cpBody* bodyPtr), $(double angle));
  } |]
  -- FIXME reindex

reindexed :: Ptr Body -> IO ()
reindexed bodyPtr = do
  [C.block| void {
    cpSpaceReindexShapesForBody($(cpBody* bodyPtr));
    } |]
  -- once per body
  pure ()
