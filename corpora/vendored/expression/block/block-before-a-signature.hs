{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module M where

a = do { pure () } :: IO ()

b = (case x of { A -> y } :: T)

c = run do { x } :: IO ()

d = x <> do { y } :: T

e = \case { A -> y } :: T -> U

f =
  do
    x
    y
    :: IO ()

g =
  run do
    x
    y
    :: IO ()

h =
  f
    x
    y ::
    Int
