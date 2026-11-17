{-# LANGUAGE CPP #-}

module Audio.Mix where

#ifdef MIXER
(<+>) :: Track -> Track -> Track
#else
blend :: Track -> Track -> Track
#endif
(Track a)
#ifdef MIXER
  <+>
#else
  `blend`
#endif
  (Track b) = Track (a <> b)
