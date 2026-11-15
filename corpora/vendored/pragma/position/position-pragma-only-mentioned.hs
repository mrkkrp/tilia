-- | Talking about @{-# LINE #-}@ is not carrying one.
module Woven.Notes where

{- A block comment may name {-# COLUMN #-} too, nested {- and all -}. -}

-- | The text a generator would emit.
marker :: String
marker = "{-# LINE 40 \"Template.hs\" #-}"

-- Nor is --> a comment opener, so a pragma after it still counts.
described :: Int
described = 1
