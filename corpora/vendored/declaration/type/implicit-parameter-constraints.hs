{-# LANGUAGE ImplicitParams #-}

module Report.Order where

import Data.List (sortBy)

ordered :: (?compare :: a -> a -> Ordering) => [a] -> [a]
ordered = sortBy ?compare

ranked :: (?compare::a -> a -> Ordering, ?limit :: Int) => [a] -> [a]
ranked = take ?limit . ordered
