module M where

f x
  | x > 0 = {- positive -}
      1
  | x < 0 = -- negative
      2
  | otherwise = {- zero,
      or close -}
      3

g x = case x of
  Just y
    | y > 0 -> {- positive -}
        y
  _ -> 0
