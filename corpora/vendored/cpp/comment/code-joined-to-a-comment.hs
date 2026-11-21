{-# LANGUAGE CPP #-}

module Network.Mail.Mime (escaped) where

import Data.Word (Word8)

escaped :: Word8 -> Bool
escaped 91 = True -- [
escaped 92 = True -- \
escaped 93 = True -- ]
escaped _ = False
