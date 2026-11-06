{-# LANGUAGE TupleSections #-}

module Queue.Pairs where

pending = (,Nothing {- yet -})

leftOnly = (Left x, {- the right comes later -})

rightOnly =
  (,Right y
  -- and the left from the caller
  )

done = (x, y)
