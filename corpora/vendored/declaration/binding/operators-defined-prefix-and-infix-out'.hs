module Vector.Plane where

infixl 6 |+|

(|+|) :: Vec -> Vec -> Vec
(|+|) (Vec a b) (Vec c d) = Vec (a + c) (b + d)

(|-|) :: Vec -> Vec -> Vec
u |-| Vec c d = u |+| Vec (negate c) (negate d)

cross :: Vec -> Vec -> Double
Vec a b `cross` Vec c d = a * d - b * c

area :: Vec -> Vec -> Double
area u v = abs (u `wedge` v) / 2
    where
        (Vec a b) `wedge` (Vec c d) = a * d - b * c
        wedge' x = (`wedge` x)
