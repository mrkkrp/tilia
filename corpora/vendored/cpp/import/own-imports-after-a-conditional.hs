{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module Weather.Report (report) where

import Data.List (sortOn)
#if defined(USE_CONTAINERS)
import Data.Map (Map)
#endif
import "this" Weather.Reading (Reading, readingTime)

report :: [Reading] -> [Reading]
report = sortOn readingTime
