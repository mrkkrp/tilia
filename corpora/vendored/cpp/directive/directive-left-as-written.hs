{-# LANGUAGE CPP #-}

-- Lines left as written that hold a directive cannot be put back in every
-- configuration, and the module is declined.
module Loom.Reeds where

reeds = 1

{- TILIA_OFF -}
#if MIN_VERSION_base(4,0,0)
dents   =   2
#else
dents   =   3
#endif
{- TILIA_ON -}
