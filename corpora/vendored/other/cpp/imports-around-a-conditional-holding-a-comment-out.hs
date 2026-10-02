{-# LANGUAGE CPP #-}

module Terminal.Size (columns) where

import System.Environment (lookupEnv)
#ifdef mingw32_HOST_OS
{- The console size comes from the environment there as well. -}
#endif
import Data.Maybe (fromMaybe)

columns :: IO Int
columns = read . fromMaybe "80" <$> lookupEnv "COLUMNS"
