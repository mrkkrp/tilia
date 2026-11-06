{-# LANGUAGE UnboxedTuples #-}

-- With unboxed tuples on, an operator that begins with # keeps the space
-- after an opening parenthesis, and # alone the space before a closing one,
-- so that neither reads as half of an unboxed tuple.
module Builder.Append
  ( ( #<| ),
    (<|#),
    ( # ),
    prepended,
  )
where

( #<| ), (<|#), ( # ) :: String -> String -> String
x #<| y = x ++ y
x <|# y = y ++ x
x # y = x ++ " " ++ y

infixr 5 #<|

prepended :: [String] -> [String]
prepended =
  map ( #<| "!") . map ("?" <|#) . map ("a" # ) . foldr (( #<| ) . pure) []
