module Stream.Fusion where

{-# RULES
"zip/unzip" [~2] forall a b. forall (ps :: [(a, b)]).
    uncurry zip (unzip ps) = ps
"scan/scan"
    scan f (scan g s) = scan (f . g) s
"drain/fill"
    drain
        n
        (fill n) = id
    #-}
