{-# LANGUAGE CPP #-}

module Weather.Station (station) where

import Data.Char (toUpper)
-- Station names are upper case.
import Data.Map.Strict (Map)
#if defined(USE_TEXT)
import Data.Text (Text)
#endif

station :: Map String Int
station = mempty
