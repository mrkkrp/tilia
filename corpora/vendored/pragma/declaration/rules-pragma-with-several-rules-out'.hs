module Vector.Fuse where

{-# RULES
"map/map" forall f g xs. map f (map g xs) = map (f . g) xs
"sum/replicate" [1] forall n (x :: Int). sum (replicate n x) = n * x
"pair/unpair" [~2] forall a b. forall (ps :: [(a, b)]). pair (unpair ps) = ps
    #-}
