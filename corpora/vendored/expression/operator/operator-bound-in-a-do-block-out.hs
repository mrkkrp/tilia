-- An operator a do block binds is in scope over the statements after it:
-- bound by an arrow it is infixl 9, and bound by a let it takes the
-- fixity declared beside it, whatever the module declares for an operator
-- of its name.
module Score.Rounds where

infixr 2 |+|

(|+|) :: Int -> Int -> Int
a |+| b = a + b

pickScorer :: IO (Int -> Int -> Int)
pickScorer = pure max

scored :: [Int] -> IO ()
scored scores = do
  (|+|) <- pickScorer
  let total =
        sum scores
          |+| length scores
          * 2
  print total

settled :: [Int] -> IO ()
settled scores = do
  let infixl 7 |+|
      a |+| b = a - b
  print (sum scores)
  print $
    sum scores
      |+| length scores
      + 2
