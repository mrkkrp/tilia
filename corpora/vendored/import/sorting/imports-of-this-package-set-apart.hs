{-# LANGUAGE PackageImports #-}

-- Imports that name "this" package come from the project itself, so they
-- are sorted into a group of their own after the others.
module Weather.Report where

import "this" Weather.Units (Celsius)
import Data.List (sortOn)
import "this" Weather.Reading (Reading, temperature)
import Data.Map.Strict (Map)

report :: Map String Reading -> [Celsius]
report = fmap temperature . sortOn temperature . foldr (:) []
