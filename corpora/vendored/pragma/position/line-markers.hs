-- | Code stitched together from several files, which says where each part
-- of it came from the way cpp does.
module Stitched where

ownLine :: Int -> Int
ownLine n = n + 1

# 40 "Template.hs"

fromTheTemplate :: Int -> Int
fromTheTemplate n = n

#line 12 "Stitched.hs"

andBackAgain :: Int -> Int
andBackAgain n = n - 1
