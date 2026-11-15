{-# LANGUAGE CPP #-}

module Terminal.Features where

supportsColour :: Bool
supportsColour  =  True

#if MIN_VERSION_base(4,20,0)
#else
#endif

supportsPaste :: Bool
supportsPaste  =  True
