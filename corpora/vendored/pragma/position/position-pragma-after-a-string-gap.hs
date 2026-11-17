-- | A string gap may end right at the closing quote.
module Woven.Gap where

origin :: String
origin = "Template.hs\ \"

{-# LINE 40 "Template.hs" #-}

fromTheTemplate :: Int -> Int
fromTheTemplate n = n
