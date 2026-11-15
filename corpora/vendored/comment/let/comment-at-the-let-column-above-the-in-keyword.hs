module Route.Return where

fare :: Int -> Int
fare single =
  let outward = single
  -- the way back costs the same
  in outward * 2
