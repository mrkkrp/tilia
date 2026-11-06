module Kiln.Firing where

holdFor :: Int -> Int -> Int
holdFor minutes degrees = minutes * degrees

-- The lines of this paragraph belong to one another, and the lexer hands
-- each of them over on its own. An empty line written over every one would
-- take the paragraph apart and set out each line as a remark of its own.

-- A second paragraph, which the empty line above it does set apart. The
-- spacing between the two is the author's, and so is the lack of it within
-- either.
