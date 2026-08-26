{-# LANGUAGE CPP #-}

module Terminal.Encode (encode) where

import Data.List (intercalate)
#if MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif
import Data.Char (ord)

encode :: String -> String
encode  =  intercalate ";" . map (show . ord)
