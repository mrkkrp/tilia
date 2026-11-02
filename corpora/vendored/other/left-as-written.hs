-- The lines from a {- TILIA_OFF -} to the next {- TILIA_ON -} come out as
-- written, comments and all, and everything around them is formatted.
module Loom.Patterns where

import Data.List (sortOn)

-- | The threads of each pattern, lined up by hand.
patterns :: [(String, [Int])]
{- TILIA_OFF -}
patterns =
    -- name          warp  weft  twist
    [ ("herringbone", [ 3,    2,    4 ])
    , ("houndstooth", [ 2,    3,    5 ])
    ]
{- TILIA_ON -}
-- the order the looms take them in

ordered   ::   [(String, [Int])]
ordered = sortOn   fst patterns

{- TILIA_OFF -}
-- A table   kept   as   it   is:
--   herringbone   twill
--   houndstooth   check
{- TILIA_ON -}

count :: Int
count =   length   ordered
