{-# LANGUAGE CPP #-}

module Shop.Merge where

instance
  (
#if !(MIN_VERSION_base(4,11,0))
    Semigroup c,
#endif
    Monoid c
  ) =>
  Merge (K1 i c)
  where
  merge = K1 mempty

combine ::
  (
#if !(MIN_VERSION_base(4,11,0))
    Semigroup a,
#endif
    Monoid a
  ) =>
  [a] -> a
combine = mconcat
