module Tally.Join where

newtype Tally = Tally Int

instance Semigroup Tally where (<>) (Tally a) (Tally b) = Tally (a + b)

instance Ord Tally where
    (<=) (Tally a) (Tally b) = a <= b
    max
        (Tally a)
        (Tally b) =
            Tally (max a b)
    (>=)
        a
        b = b <= a
