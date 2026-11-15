{-# LANGUAGE CPP #-}

module Terminal.Cache (empty) where

import Data.Char (isSpace)
import
#ifdef STRICT_CACHE
  qualified
#endif
  Data.Map as M
import Text.Read (readMaybe)

empty :: M.Map String String
empty = M.empty
