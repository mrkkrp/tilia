{-# LANGUAGE RoleAnnotations #-}

module Unit.Measure where

type role Quantity phantom representational

newtype Quantity u a = Quantity a

type role Tagged nominal _ phantom

data Tagged k a b = Tagged k a
