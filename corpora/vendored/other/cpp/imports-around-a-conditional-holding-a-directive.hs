{-# LANGUAGE CPP #-}

module Terminal.Width (width) where

import System.Environment (lookupEnv)
#ifdef WIDE_TERMINALS
  #define DEFAULT_WIDTH 132
#endif
import Data.Maybe (fromMaybe)

width :: IO Int
width  =  read . fromMaybe "80" <$> lookupEnv "COLUMNS"
