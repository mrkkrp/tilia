{-# LANGUAGE CPP #-}

module Terminal.Features where

supportsColour :: Bool
supportsColour  =  True

#ifdef WINDOWS
-- The console has no bracketed paste mode.
#endif

supportsPaste :: Bool
supportsPaste  =  True
