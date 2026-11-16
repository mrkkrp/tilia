{-# LANGUAGE UnboxedSums #-}

module Lexer.Width where

narrow :: (# Int | Char #)
narrow = (# 7 | #)

wide
    :: (#
        Int |
        Char |
        Bool |
        () |
        Double
       #)
wide =
    (#
        | | 2.5 | |
    #)
