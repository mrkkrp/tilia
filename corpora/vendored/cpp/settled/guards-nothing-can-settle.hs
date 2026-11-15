{-# LANGUAGE CPP #-}
#if MIN_TOOL_VERSION_ghc(9,6,0)
{-# LANGUAGE TypeData #-}
#endif
module Orchard.Fruit where

#if MIN_TOOL_VERSION_ghc(9,6,0)
type  data  Season  =  Spring  |  Autumn
#else
data  Season  =  Spring  |  Autumn
#endif

#if MIN_TOOL_VERSION_ghc(9,4,0) && ORCHARD_FEATURES >= 0x0100
ripe :: Int
ripe  =  1
#endif

#if HAS_FEATURE(FLAG(PRUNED))
pruned :: Bool
pruned  =  True
#endif
