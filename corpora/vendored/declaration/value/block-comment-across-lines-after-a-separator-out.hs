module Ledger.Entry where

amount entry = case entry of
  Credit n ->
    {- paid in,
    as written -}
    n
  Debit n -> negate n

signed = \n ->
  {- the sign
  is the caller's -}
  n

opening =
  {- carried over
  from last year -}
  0
