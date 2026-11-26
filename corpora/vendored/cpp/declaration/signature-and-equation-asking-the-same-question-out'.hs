{-# LANGUAGE CPP #-}

module Audio.Mix where

#ifdef MIXER
(<+>) :: Track -> Track -> Track
#else
blend :: Track -> Track -> Track
#endif
(Track a)
#ifdef MIXER
    <+> (Track b) =
#else
    `blend` (Track b) =
#endif
        Track (a <> b)
