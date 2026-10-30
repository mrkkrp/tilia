#!/usr/bin/env stack
-- stack script --snapshot lts-22.21 --package text

-- Print each line of the input with its words in reverse order.
module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = T.interact (T.unlines . map (T.unwords . reverse . T.words) . T.lines)
