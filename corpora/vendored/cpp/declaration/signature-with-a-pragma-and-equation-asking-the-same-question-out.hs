{-# LANGUAGE CPP #-}

module Audio.Mix where

#ifdef MIXER
(<+>) :: Track -> Track -> Track
{-# INLINE (<+>) #-}
#else
blend :: Track -> Track -> Track
{-# INLINE blend #-}
#endif
(Track a)
#ifdef MIXER
  <+> (Track b) =
#else
  `blend` (Track b) =
#endif
    Track (a <> b)
