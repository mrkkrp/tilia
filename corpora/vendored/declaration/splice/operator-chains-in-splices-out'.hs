{-# LANGUAGE TemplateHaskell #-}

module Schema.Tables where

import Schema.Derive (deriveColumns, deriveTable)

concat <$> traverse deriveTable [''Account, ''Ledger]

concat
    <$> traverse deriveTable [''Invoice, ''Payment]

(<>)
    <$> deriveTable ''Refund
    <*> deriveColumns ''Refund

$( concat
    <$> traverse deriveColumns [''Account, ''Ledger]
 )
