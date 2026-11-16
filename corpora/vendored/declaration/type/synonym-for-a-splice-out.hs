{-# LANGUAGE TemplateHaskell #-}

module Sql.Row where

type Row = $(tupleOf 3 [t|Int|])

type Key = $keyType
