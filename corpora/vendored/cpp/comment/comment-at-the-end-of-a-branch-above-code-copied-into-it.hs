{-# LANGUAGE CPP #-}

module Queue.MichaelScott where

pop queue = do
  next <- readNext queue
#ifdef RECHECK_ASSUMPTIONS
  if consistent next then pop queue else do
    -- Only a consistent snapshot is worth finishing.
#else
  do
#endif
    finish next

push queue x =
#ifdef RECHECK_ASSUMPTIONS
  if full queue then push queue x else
    -- A full queue is retried as it stands.
#endif
    store queue x

peek :: Queue -> IO (Maybe Int)
peek queue =
#ifdef RECHECK_ASSUMPTIONS
  mfilter (not . stale) <$>
  -- What is stale goes, whatever the head says.
#endif
  readHead queue
