{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Audio.Mix where

#ifdef MIXER
type Mixer :: Type
#else
type Blender :: Type
#endif
data
#ifdef MIXER
    Mixer
#else
    Blender
#endif
    = Track
