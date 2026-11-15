module Route.Split where

split :: Int -> Int
split total =
  let outward = total `div` 2
      -- whatever is left over rides home
      homeward = total - outward
   in homeward
