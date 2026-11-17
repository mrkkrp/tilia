{-# LANGUAGE CPP #-}

module Shop.Plus where

class Plus f where
    zero :: f a

instance
    ( Monoid c
#if !(MIN_VERSION_base(4,11,0))
    , Semigroup c
#endif
    )
    => Plus (K1 i c)
    where
    zero = K1 mempty

render
    :: ( Show a
#if defined(DEBUG)
       , Eq a
#endif
       )
    => a -> String
render = show
