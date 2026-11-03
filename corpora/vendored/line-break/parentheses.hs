-- Parentheses around a function application break together with it, and
-- those around an operator chain keep their line.
module Loom.Parentheses where

finish loom = tie (bind (edgeThreads loom) (selvedgeWidth defaultLoom) (selvedgeTension defaultLoom)) loom

knot loom = tie (bind (edgeThreads loom) $ selvedgeWidth defaultLoom + selvedgeTension defaultLoom * 2) loom
