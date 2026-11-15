{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TransformListComp #-}

module Census.Tally where

import Data.List (inits)
import GHC.Exts (groupWith, sortWith, the)

oldest people =
  [name | (name, age) <- people, then sortWith by negate age, then take 3]

byTown ps = [(the t, sum a) | (t, a) <- ps, then group by t using groupWith]

prefixes xs = [x | x <- xs, then group using inits]

ranked ns ss = [(n, s) | n <- ns, then sortWith by n | s <- ss, then reverse]
