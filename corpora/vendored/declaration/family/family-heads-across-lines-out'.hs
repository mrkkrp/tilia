{-# LANGUAGE TypeFamilyDependencies #-}

module Ledger.Book where

import Data.Kind (Type)

type family
    Balance
        account
        currency

type family
    Posting account
        :: Type

data family
    Journal
        entry
        period
        :: Type -> Type

type family
    Column sheet =
        column | column -> sheet

type family
    Rate from to
        :: Type
    where
    Rate from from = Int
    Rate from to = Double
