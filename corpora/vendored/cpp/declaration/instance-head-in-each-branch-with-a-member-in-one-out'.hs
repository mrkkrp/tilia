{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Kitchen.Pot where

instance
#ifdef WITH_TIMERS
    Cooks (t -> Stew) where
    type Timer (t -> Stew) = t
#else
    Cooks (() -> Stew) where
#endif
    cook f = simmer (f ())
