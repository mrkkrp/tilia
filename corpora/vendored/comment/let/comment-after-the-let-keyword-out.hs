module Route.Cost where

leg :: Int -> Int
leg n =
  let -- charged once, whatever the distance
      base = 40
      perStop = 7
   in base + perStop * n
