module Route.Clamp where

stops :: Int -> Int -> Int
stops wanted total =
  let served = min wanted total
        -- never more than the line has
   in served + 1
