{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module Weather.Report (report) where

import Data.List (sortOn)
import "this" Weather.Reading (Reading, readingTime)
#if defined(USE_CONTAINERS)
import Data.Map (Map)
import "this" Weather.Station (Station)
#endif

report :: [Reading] -> [Reading]
report = sortOn readingTime
