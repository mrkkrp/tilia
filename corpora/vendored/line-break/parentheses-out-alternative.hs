-- Parentheses break together with what they hold, and keep their line
-- around what cannot break.
module Loom.Parentheses where

finish loom =
  tie (bind (edgeThreads loom) (selvedgeWidth defaultLoom) (selvedgeTension defaultLoom)) loom

knot loom =
  tie (bind (edgeThreads loom) $ selvedgeWidth defaultLoom + selvedgeTension defaultLoom * 2) loom

label = describe ("a description of the loom, long enough that it cannot fit on any line at all")
