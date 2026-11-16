-- An operator bound by an argument, a lambda or a pattern guard is
-- infixl 9 where it is in scope, whatever the module declares for an
-- operator of its name.
module Score.Tally where

infixr 2 |+|

(|+|) :: Int -> Int -> Int
a |+| b = a + b

tally :: Int -> Int -> Int -> Int
tally x y z =
  x
    |+| y
      * z

tallyWith :: (Int -> Int -> Int) -> Int -> Int -> Int -> Int
tallyWith (|+|) x y z =
  x
    |+| y
    * z

tallyBy :: (Int -> Int -> Int) -> Int -> Int -> Int -> Int
tallyBy = \(|+|) x y z ->
  x
    |+| y
    * z

tallyIf :: Maybe (Int -> Int -> Int) -> Int -> Int -> Int -> Int
tallyIf scorer x y z
  | Just (|+|) <- scorer =
      x
        |+| y
        * z
  | otherwise = 0
