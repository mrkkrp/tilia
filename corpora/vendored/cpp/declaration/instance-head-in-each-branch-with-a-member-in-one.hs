{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Kitchen.Pot where

#ifdef WITH_TIMERS
instance Cooks (t -> Stew) where
  type Timer (t -> Stew) = t
#else
instance Cooks (() -> Stew) where
#endif
  cook f = simmer (f ())
