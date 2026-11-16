{-# LANGUAGE CPP #-}

module Kitchen.Compat (boil, simmer) where

import Kitchen.Heat (boil)

#ifdef MODERN_STOVE
import Kitchen.Heat (simmer)
#else
#ifdef GAS
#if GAS_VERSION >= 3
import Kitchen.Gas (simmer)
#else
simmer :: Pot -> Pot
simmer = boil
#endif
#else
#if ELECTRIC_VERSION >= 2
import Kitchen.Electric (simmer)
#else
simmer :: Pot -> Pot
simmer = boil
#endif
#endif
#endif
