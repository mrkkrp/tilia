{-# LANGUAGE TemplateHaskell #-}

-- A comment before the bracket that closes a quote stays inside the quote.
expression =
  [|
    lookup key table
    -- or a default, when the key is missing
    |]
    ++ fallbacks

typed =
  [||
  width * height
  -- in pixels
  ||]

declarations =
  [d|
    answer = 42
    -- and that is all
    |]
