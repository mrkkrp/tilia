{-# LANGUAGE PatternSynonyms #-}

-- The fields of a pattern synonym that run past column 80 are broken one
-- per line, and what it stands for stays after its = where it fits.
module Loom.Point where

pattern Point
    { pointLeft
    , pointRight
    , pointAbove
    , pointBelow
    , pointDepth
    , pointHeight
    } = (pointLeft, pointRight)
