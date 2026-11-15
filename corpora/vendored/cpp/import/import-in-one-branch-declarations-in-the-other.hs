{-# LANGUAGE CPP #-}
module Terminal.Sort where

import Data.Char (toLower)

#if MIN_VERSION_base(4,8,0)
import Data.List (sortOn)
#else
sortOn   ::  Ord b => (a -> b) -> [a] -> [a]
sortOn _  =  id
#endif

byName :: [String] -> [String]
byName  =  sortOn (fmap toLower)

byLength :: [String] -> [String]
byLength  =  sortOn length
