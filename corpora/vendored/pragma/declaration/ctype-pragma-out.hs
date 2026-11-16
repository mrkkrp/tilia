{-# LANGUAGE CApiFFI #-}

module Terminal.Attributes where

import Data.Word (Word32)

data {-# CTYPE "termios.h" "struct termios" #-} Termios

data {-# CTYPE "sys/ioctl.h" "struct winsize" #-} WindowSize

newtype {-# CTYPE "useconds_t" #-} Micros = Micros Word32
