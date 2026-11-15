-- A {- TILIA_OFF -} inside a declaration is not honoured, and the module is
-- declined.
module Loom.Heddles where

heddles = threaded
  where
{- TILIA_OFF -}
    threaded   =   [ 1 ,   2 ]
{- TILIA_ON -}
