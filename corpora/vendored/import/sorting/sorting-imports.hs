{-# LANGUAGE PackageImports #-}

module Weather.Station where

import Prelude hiding (lookup)
import "this" Weather.Units (Celsius)
import Data.Map.Strict (Map)
import "containers" Data.Set (Set)
import Control.Monad (forM_)
import qualified Data.Map as Map
import Data.Map (lookup)
import "base" Data.List (sort)
import Weather.Reading (Reading)
import Data.Char (toUpper)

readings :: Map String Reading
readings = mempty
