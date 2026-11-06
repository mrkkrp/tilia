module Route.Plan where

plan :: IO Int
plan = do
  let -- worked out before anything is printed
      stops = 9
  print stops
  pure stops
