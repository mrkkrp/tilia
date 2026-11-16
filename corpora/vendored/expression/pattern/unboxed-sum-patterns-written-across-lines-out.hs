{-# LANGUAGE UnboxedSums #-}

module Lexer.Slot where

firstSlot :: Bool
firstSlot = ready
  where
    (# ready | #) = (# True | #)

lastSlot :: Bool
lastSlot = ready
  where
    (#
      | | ready
      #) =
        (#
          | | False
        #)
