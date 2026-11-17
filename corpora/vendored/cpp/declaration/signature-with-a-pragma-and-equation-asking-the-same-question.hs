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
  <+>
#else
  `blend`
#endif
  (Track b)  =  Track (a <> b)
