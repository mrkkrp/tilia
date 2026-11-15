{-# LANGUAGE CPP #-}

module Codec.Frames where

#if defined(DOCS)
-- | The fields every frame opens with.
#endif
#if defined(FRAMES)
header = 1

trailer = 2
#endif
#if defined(FRAMES)
padding = 3
checksum = 4
#endif
