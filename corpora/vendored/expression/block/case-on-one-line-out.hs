module Queue.Peek where

single :: [a] -> Maybe a
single xs = case xs of [x] -> Just x; _ -> Nothing

describe :: [a] -> String
describe xs = case xs of { [_, _] -> "a pair"; _ -> "something" } ++ "."

lowest :: Int
lowest = case [3, 1, 2] of (x : _) -> x; [] -> 0
