module Route.Trim where

trim :: Int -> Int
trim n =
  let kept = n - 1
   in -- rounded down on purpose
      kept
