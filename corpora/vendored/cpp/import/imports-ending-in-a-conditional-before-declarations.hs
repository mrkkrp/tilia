{-# LANGUAGE CPP #-}

module Terminal.Width (width) where

import Data.Char (isSpace)
#ifdef WITH_WCWIDTH
import Terminal.Wcwidth (wcwidth)
#endif
width :: String -> Int
width  =  length . filter (not . isSpace)
