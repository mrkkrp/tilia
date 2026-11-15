{-# LANGUAGE CPP #-}

module Terminal.Cache (empty) where

import Text.Read (readMaybe)
import
#ifdef STRICT_CACHE
  qualified
#endif
  Data.Map as M
import Data.Char (isSpace)

empty :: M.Map String String
empty  =  M.empty
