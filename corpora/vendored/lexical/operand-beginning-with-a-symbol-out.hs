{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

-- A prefix minus or a backslash stays apart from what follows it where that
-- begins with a symbol character, as the two would read as one operator.
module Ledger.Balance where

debit = - ?amount

credit = - ?rate * ?amount

refund = - #total

drift = - ?rate interval

offset = - $(lookupOffset)

typedOffset = - $$(lookupOffset)

spliced = - $offset

parity = map (\ -1 -> 0) readings

unboxed = \ -1# -> 0#
