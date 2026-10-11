{-# LANGUAGE CPP #-}

module Kiln.Batch (batch) where

-- The first let of a chain, written in each branch with in at the end of
-- its line, stays in each branch as written.
batch :: Int -> Int -> Int
batch x s =
#if defined(LARGE)
  let a = large x in
#else
  let a = small x in
#endif
  let b = s + a
   in scale b
