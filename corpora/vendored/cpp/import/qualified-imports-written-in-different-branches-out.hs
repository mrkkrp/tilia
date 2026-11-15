{-# LANGUAGE CPP #-}

module Terminal.Text (strip) where

import Text.Read (readMaybe)
#ifdef WITH_LAZY_TEXT
import qualified Data.Text.Lazy as T
#else
import qualified Data.Text as T
#endif
  hiding (lines)
import Data.Char (isSpace)

strip :: T.Text -> T.Text
strip = T.strip
