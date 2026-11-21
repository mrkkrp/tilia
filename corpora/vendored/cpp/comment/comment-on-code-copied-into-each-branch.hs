{-# LANGUAGE CPP #-}

module Queue.MichaelScott where

pop queue = do
  next <- readNext queue
#ifdef RECHECK_ASSUMPTIONS
  if consistent next then pop queue else do
#else
  let checked = next
  do
#endif
    finish next -- taken more than a million times

push queue x =
#ifdef RECHECK_ASSUMPTIONS
  if full queue then push queue x else
#endif
    store queue x -- in place
