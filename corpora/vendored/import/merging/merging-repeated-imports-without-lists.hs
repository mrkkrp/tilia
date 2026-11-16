module Garden.Rows where

import Data.List
import Data.Char
import Data.List

rows :: [String] -> [String]
rows = sort . map (map toUpper)
