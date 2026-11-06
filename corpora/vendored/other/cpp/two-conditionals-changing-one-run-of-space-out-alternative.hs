{-# LANGUAGE CPP #-}

-- Taking the #else of the first conditional and the branch of the second,
-- each adds to the space between the first Haddock and the heading, the
-- second nearer the Haddock, which documents what it adds.
module Codec.Frame where

#if FAST
#else
-- | Decoding a frame.
#if MIN_VERSION_bytestring(0,11,0)
#else
-- | Decoding a frame, copying it first.
#endif
#endif
#if FAST
#if MIN_VERSION_bytestring(0,11,0)
#else
decode = copy
#endif
#endif

-- * Encoding
