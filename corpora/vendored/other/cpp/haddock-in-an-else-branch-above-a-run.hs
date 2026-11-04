{-# LANGUAGE CPP #-}

module Codec.Frames where

#if defined(TERSE)
#else
-- | The fields every frame opens with.
#endif
header = 1
trailer = 2

version = 1
