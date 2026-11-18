{-# LANGUAGE CPP #-}

module Audio.Queue where

-- A bind whose pattern is written infix, with the operator in each branch
-- and a do block hanging after the arrow.
next :: Queue -> IO Track
next queue = do
  track
#if MIN_VERSION_containers(0,7,0)
    :< rest <-
#else
    :| rest <-
#endif
    do
      pop queue
  play rest
  pure track
