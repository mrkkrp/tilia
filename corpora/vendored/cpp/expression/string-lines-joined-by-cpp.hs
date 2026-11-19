{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module Table (digits, separator) where

import GHC.Exts (Addr#)

#if defined(mingw32_HOST_OS)
separator :: Char
separator = '\\'
#else
separator :: Char
separator = '/'
#endif

digits :: Addr#
digits = "\x00\x01\x02\x03\
        \\x04\x05\x06\x07"#
