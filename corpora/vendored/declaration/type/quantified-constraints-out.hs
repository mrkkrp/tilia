{-# LANGUAGE QuantifiedConstraints #-}

module Lift.Compare where

equalBoth :: (forall x. Eq x => Eq (f x)) => f Int -> f Int -> Bool
equalBoth = (==)

describeBoth ::
  ( forall x. Show (f x),
    forall x.
    Show x =>
    Show (g x)
  ) =>
  f Int -> g Int -> String
describeBoth a b = show a ++ show b
