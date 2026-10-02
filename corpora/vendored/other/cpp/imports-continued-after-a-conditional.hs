{-# LANGUAGE CPP #-}

module Terminal.Lines (wrap) where

import Text.Read (readMaybe)
#ifdef WITH_TEXT
import Data.Text
#else
import Data.List
#endif
#if MIN_VERSION_base(4,20,0)
    hiding (lines)
#endif
import Data.Char (isSpace)

wrap :: Int -> String -> [String]
wrap n  =  map (take n) . Prelude.lines
