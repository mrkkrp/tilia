{-# LANGUAGE CPP #-}

module Codec.Frames where

#if defined(CHECKED)
-- | The checksum of a frame.
#endif
#if MIN_VERSION_base(4,8,0)
#if defined(CHECKED)
checksum = 0
#endif
header = 1
trailer = 2
#endif
