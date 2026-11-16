{-# LANGUAGE InstanceSigs #-}

module Tally.Show where

newtype Tally = Tally Int

instance Show Tally where
    show :: Tally -> String
    show (Tally n) = replicate n '|'
    showsPrec
        :: Int
        -> Tally
        -> ShowS
    showsPrec _ t = showString (show t)

instance Eq Tally where
    (==) :: Tally -> Tally -> Bool
    Tally a == Tally b = a == b
