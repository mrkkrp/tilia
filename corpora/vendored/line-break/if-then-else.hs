-- An if that runs past column 80 is broken before its then and its else.
module Loom.Ifs where

shuttle loom = if loomWidth loom > shuttleReach then throwFromBothSides loom else throwOnce loom
