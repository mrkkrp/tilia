{-# LANGUAGE CPP #-}

module Bakery.Loaf (Loaf (..)) where

data Loaf = Loaf
    { flour :: !Int
#ifdef WITH_SALT
    , salt :: !Int
#endif
#ifdef WITH_YEAST
    , yeast :: !Int
#endif
    , water :: !Int
    }
