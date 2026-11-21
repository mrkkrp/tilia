{-# LANGUAGE CPP #-}

module Queue.MichaelScott where

pop queue = do
  next <- readNext queue
#ifdef RECHECK_ASSUMPTIONS
  if consistent next
    then pop queue
    else do
      finish next -- taken more than a million times
#else
  let checked = next
  do
    finish next -- taken more than a million times
#endif

push queue x =
#ifdef RECHECK_ASSUMPTIONS
  if full queue
    then push queue x
    else
      store queue x -- in place
#else
  store queue x -- in place
#endif
