module Ledger.Rewrite where

import Data.Kind (Type)

{-# RULES "put/id" forall (c :: Type). forall (e :: Entry c). put id e = e #-}

{-# RULES
"fold/entries" forall (c :: Type) (m :: Type -> Type). forall
  (step :: forall r. Entry c -> r -> m r)
  (es :: Ledger c).
  foldEntries step (entries es) =
    walk step es
"settle/apply" forall
  ( f ::
      Entry c -> Entry c
  )
  (l :: Ledger c).
  settle (apply f l) =
    \k -> settleWith k f l
  #-}
