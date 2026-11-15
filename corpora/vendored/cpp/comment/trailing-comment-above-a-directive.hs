{-# LANGUAGE CPP #-}

module Settings (settings) where

import Options (Options (..), defaultOptions)

settings :: Options
settings =
  defaultOptions
    { optionA = True
    , -- Highlighting is on by default, as it was
      -- in earlier releases.
#if MIN_VERSION_base(4,20,0)
      optionB = True
#else
      optionB = False
#endif
    }
