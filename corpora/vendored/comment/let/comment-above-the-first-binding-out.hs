module Route.Fare where

fare :: Int -> Int
fare riders =
  let
      -- the share the operator keeps
      operator = 3
      levy = 1
   in operator * riders + levy
