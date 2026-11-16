{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Pantry.Jar where

type family Label a

#ifdef WITH_KINDS
type instance Label (Jar a) =
#else
type instance Label Jar =
#endif
  JarLabel
