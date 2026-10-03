{-# LANGUAGE TypeOperators #-}

-- An operator written at the end of a line goes to the start of the next,
-- and a comment written after it stays with the operand it follows.
type Shape =
  Circle -- round
    :+: Square -- angular
    :+: Segment -- and flat
