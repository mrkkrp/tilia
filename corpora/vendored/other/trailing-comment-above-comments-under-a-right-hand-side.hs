module Matching where

matches :: Int -> Int -> Bool
matches a b = a == b -- quick test
              -- and not the full one

instance Eq Ledger where
  a == b = name a == name b  -- names only
           -- the rest follows from them

render :: Int -> [Int]
render x = case x of
   0   -> [1]  -- a zero-width space
          -- to avoid an empty box
   _ -> []
