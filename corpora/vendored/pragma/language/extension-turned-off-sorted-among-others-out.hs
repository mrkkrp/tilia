{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}

module Garden.Pairs where

-- Without the restriction, the binding below stays polymorphic.
paired = (,True)
