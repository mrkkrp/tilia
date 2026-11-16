{-# LANGUAGE RecursiveDo #-}

module Circuit.Latch where

latch :: Signal -> Circuit Signal
latch input = do
  rec q <- nand input qBar

      qBar <- nand input q
  probe q
  rec echo <- buffer echo
  pure q
