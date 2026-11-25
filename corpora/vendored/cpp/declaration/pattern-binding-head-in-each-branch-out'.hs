{-# LANGUAGE CPP #-}

module Codec.Table (decodeTable) where

decodeTable :: ForeignPtr Word8
#if MIN_VERSION_bytestring(0,11,0)
BS decodeTable _ =
#else
PS decodeTable _ _ =
#endif
    B.pack $
        replicate 43 x
            ++ [62, x, x, x, 63]
