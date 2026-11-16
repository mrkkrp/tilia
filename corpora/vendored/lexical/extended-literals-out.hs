{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}

module Packet.Header where

import GHC.Exts

version :: Word8#
version = 4#Word8

flags :: Word32#
flags = 0x0000ff00#Word32

checksum :: Int64#
checksum = -0b1010#Int64

port :: Int16#
port = 8080#Int16
