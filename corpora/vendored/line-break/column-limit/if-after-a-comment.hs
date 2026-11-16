-- The condition of an if goes under the if, a step in, where a comment
-- ends the line of the if, and is broken there where it runs past column
-- 80.
module Search.Proof where

tryAll xs = do
  holes <- getHoles
  if -- anything but the top is fine for a hole
     (holesOK holes (binderTy binder) && (null psnames || name `elem` psnames || allowed))
     then try xs
     else tryAll xs
