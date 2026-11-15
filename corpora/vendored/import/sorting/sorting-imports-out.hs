{-# LANGUAGE PackageImports #-}

module Weather.Station where

import Control.Monad (forM_)
import Data.Char (toUpper)
import Data.Map (lookup)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Weather.Reading (Reading)
import "base" Data.List (sort)
import "containers" Data.Set (Set)
import Prelude hiding (lookup)

import "this" Weather.Units (Celsius)

readings :: Map String Reading
readings = mempty
