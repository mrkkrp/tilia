{-# LANGUAGE CPP #-}

module Queue.MichaelScott where

pop queue = do
  next <- readNext queue
#ifdef RECHECK_ASSUMPTIONS
  if consistent next
    then pop queue
    else do
      -- Only a consistent snapshot is worth finishing.
      finish next
#else
  do
    finish next
#endif

push queue x =
#ifdef RECHECK_ASSUMPTIONS
  if full queue
    then push queue x
    else
      -- A full queue is retried as it stands.
      store queue x
#else
  store queue x
#endif

peek :: Queue -> IO (Maybe Int)
peek queue =
#ifdef RECHECK_ASSUMPTIONS
  mfilter (not . stale)
    -- What is stale goes, whatever the head says.
    <$> readHead queue
#else
  readHead queue
#endif
