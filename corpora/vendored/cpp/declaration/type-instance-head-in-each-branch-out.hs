{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Pantry.Jar where

type family Label a

type instance
#ifdef WITH_KINDS
  Label (Jar a) =
#else
  Label Jar =
#endif
    JarLabel
