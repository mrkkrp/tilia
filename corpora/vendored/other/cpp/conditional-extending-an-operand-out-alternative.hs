{-# LANGUAGE CPP #-}

module Main (main) where

import qualified Properties
import Test.QuickCheck (quickCheck)

main :: IO ()
main =
  mapM_ quickCheck $
    Properties.basic
#if __GLASGOW_HASKELL__ >= 800
      ++ Properties.nonEmpty
#endif
