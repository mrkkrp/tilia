module Route.Final where

final :: Int -> Int
final n =
  let first = n
      second = first + 1 -- the one the caller actually gets
   in second
