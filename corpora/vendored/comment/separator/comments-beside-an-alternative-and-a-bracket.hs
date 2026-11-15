module M where

atTheEnd = case x of
  a -> -- why
    b

onItsOwnLine = case x of
  a ->
    -- why
    b

beforeTheClosingBracket =
  [ a,
    b
    -- and that is all
  ]
