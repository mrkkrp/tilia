{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Route.Leg where

newtype Leg = Leg Int
    deriving
#ifdef WITH_NEWTYPE_DERIVING
        newtype
#else
        stock
#endif
        (Eq)

deriving
#ifdef WITH_STOCK
    stock
#endif
    instance
    Show Leg
