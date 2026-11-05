{-# LANGUAGE QuasiQuotes #-}

-- A comment under a quasi-quotation written over several lines stays out of
-- it.
module Physics.Body (setAngle, setMass, reindexed) where

setAngle :: Ptr Body -> Double -> IO ()
setAngle bodyPtr angle = [C.block| void {
  cpBodySetAngle($(cpBody* bodyPtr), $(double angle));
  } |]
  -- FIXME reindex

setMass :: Ptr Body -> Double -> IO ()
setMass bodyPtr mass = [C.block| void {
    cpBodySetMass($(cpBody* bodyPtr), $(double mass));
    } |]
    -- FIXME rescale

reindexed :: Ptr Body -> IO ()
reindexed bodyPtr = do
  [C.block| void {
    cpSpaceReindexShapesForBody($(cpBody* bodyPtr));
    } |]
    -- once per body
  pure ()
