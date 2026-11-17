{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Shop.Ledger where

import Data.Kind (Constraint)

total :: Num a => [a] -> a
total = sum

balance :: Ord a => a -> a -> a
balance = max

audit :: (Show a, Eq a) => a -> String
audit = show

settle :: a ~ Int => a -> a
settle = id

opened :: () => Int
opened = 0

rated :: (?rate :: Double) => Double -> Double
rated = (* ?rate)

kept :: (c :: Constraint) => Int
kept = 0

lifted :: (forall x. Eq x => Eq (f x)) => f Int -> Bool
lifted = undefined

implied :: (Eq a => Show a) => a -> String
implied = undefined

class Monoid a => Ledger a

instance Show a => Ledger [a]

instance
    ( Show a
    )
    => Ledger (Maybe a)

settled
    :: ( Monad m
       )
    => m ()
settled = pure ()
