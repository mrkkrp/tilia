{-# LANGUAGE TemplateHaskell #-}

-- The body of a typed quote is indented under its bracket, as that of an
-- untyped one is.
untyped :: (Quote m) => m Exp
untyped =
  [|
    let x = 1
     in x + x
    |]

typed :: (Quote m) => Code m Int
typed =
  [||
    let x = 1
     in x + x
    ||]

explicit :: (Quote m) => Code m Int
explicit =
  [e||
    case lookup key table of
      Just x -> x
      Nothing -> 0
    ||]
