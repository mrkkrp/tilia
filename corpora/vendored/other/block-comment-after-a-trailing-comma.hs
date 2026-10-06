module Terminal.Keys (Key, keys, {- modifiers -}) where

import Data.Array (Array, (!), bounds, {- assocs -})
import Data.List (sortOn, nub,{- group -})
