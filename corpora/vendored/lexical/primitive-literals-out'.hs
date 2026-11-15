{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Buffer.Raw where

import GHC.Exts

greeting :: Addr#
greeting = "hello, world"#

spread :: Addr#
spread =
    "first line\n\
    \second line"#

sizes :: (# Int#, Word#, Double#, Char# #)
sizes = (# 4#, 8##, 0.5##, 'x'# #)
