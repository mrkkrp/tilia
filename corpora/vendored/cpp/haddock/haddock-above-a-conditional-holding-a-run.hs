{-# LANGUAGE CPP #-}

module Codec.Frames where

-- | The fields every frame opens with.
#if defined(FRAMES)
header = 1
trailer = 2
#endif
