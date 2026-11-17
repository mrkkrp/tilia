{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

module Shop.Stock where

class
  ( Monoid s
#if !(MIN_VERSION_base(4,11,0))
    , Semigroup s
#endif
  ) =>
  Stock s
  where
  empty :: s

data Item
  = forall a.
    ( Show a
#if defined(DEBUG)
      , Eq a
#endif
    ) =>
    Item a

data Entry a where
  Entry ::
    ( Show a
#if defined(DEBUG)
      , Eq a
#endif
    ) =>
    a -> Entry a
