-- A comment lined up with the type of a strict or lazy field stays lined up
-- with it, and one written above a field's pragma stays above the pragma.
module Kiln.Load where

data Load
  = Shelves
      !Int
       -- counted from the floor up
      ~Int
       -- left alone until the kiln is stacked
      Int
  | Posts
      -- one per corner
      {-# UNPACK #-} !Int
      !Int
       -- the last of them
