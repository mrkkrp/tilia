module Vector.Fold where

zipWithOp :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithOp (<+>) (x : xs) (y : ys) = x <+> y : zipWithOp (<+>) xs ys
zipWithOp _ _ _ = []

both :: (a -> a -> a) -> a -> a -> (a, a -> a -> a)
both (<+>)@combine x y = (x <+> y, combine)

normOf :: Pair -> Double
normOf ((:&) x y) = sqrt (x * x + y * y)
