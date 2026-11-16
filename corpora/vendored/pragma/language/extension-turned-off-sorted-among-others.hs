{-# LANGUAGE TupleSections, NoMonomorphismRestriction #-}
module Garden.Pairs where

-- Without the restriction, the binding below stays polymorphic.
paired = (,True)
