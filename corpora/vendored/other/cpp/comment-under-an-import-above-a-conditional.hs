{-# LANGUAGE CPP #-}

module Weather.Station (station) where

import Data.Map.Strict (Map)
import Data.Char (toUpper)
-- Station names are upper case.

#if defined(USE_TEXT)
import Data.Text (Text)
#endif

station :: Map String Int
station = mempty
