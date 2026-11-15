primitives :: [Maybe Int] -> Int
primitives args = case () of
  _
    | [ Just a
      , Just b
      ] <- args
    -> a + b
    | otherwise -> 0

hdlTemplate :: [Maybe Int] -> Int
hdlTemplate args
  | [ Just a
    , Just b
    ] <- args = a + b
  | otherwise = 0

fromArguments :: IO [Maybe Int] -> IO Int
fromArguments getArgs = do
  [ Just a
    , Just b
    ] <- getArgs
  pure (a + b)
