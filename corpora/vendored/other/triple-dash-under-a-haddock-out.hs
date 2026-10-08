module Data.Window where

-- | Give a window over a list. GHC ends it at a line beginning with three
-- dashes, so nothing needs to hold the lines from there on off it.
--
-- @
-- window4
---  :: [Int]
--   -> [Int]
-- window4 = window
-- @
window :: [Int] -> [Int]
window = id

-- | All the windows.
---------------------------------------------------------------------------
windows :: [[Int]]
windows = []
