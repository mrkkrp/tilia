{-# LANGUAGE NoExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
module Kiln.Rank where

-- RankNTypes turns ExplicitForAll back on, so with it named last, a forall
-- can be written.
apply :: (forall a. a -> a) -> (Int, Bool)
apply f = (f 1, f True)
