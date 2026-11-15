{-# LANGUAGE CPP #-}

module Text.Wrap (wrap) where

#if defined(USE_TEXT)
import Data.Text (Text)
#endif

-- Splitting into lines.

import Data.List (unfoldr)
import Data.Char (isSpace)

wrap :: Int -> String -> [String]
wrap n = unfoldr (\s -> if null s then Nothing else Just (splitAt n s))
