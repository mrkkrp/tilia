module Vector.Pair where

data Pair = (:&) {(<.) :: Double, (.>) :: Double}

origin :: Pair
origin = (:&) {(<.) = 0, (.>) = 0}

swapped :: Pair -> Pair
swapped ((:&) {(<.) = x, (.>) = y}) = (:&) {(<.) = y, (.>) = x}
