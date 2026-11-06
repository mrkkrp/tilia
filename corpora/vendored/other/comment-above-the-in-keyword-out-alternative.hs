module Route.Total where

total :: Int -> Int
total stops =
  let each = 12
      -- the sum every caller ends up wanting
   in each * stops
