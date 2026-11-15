{-# LANGUAGE Arrows #-}

module Cache.Lookup where

-- An operator written at the end of a line goes to the start of the next,
-- and a comment written after it stays with the operand it follows.
fetch local remote = proc key ->
  (local -< key) -- cheap when it hits
    <+> (remote -< key)
