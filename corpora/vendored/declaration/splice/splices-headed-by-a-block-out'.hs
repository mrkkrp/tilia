{-# LANGUAGE TemplateHaskell #-}

module Store.Derived where

import Store.Derive (deriveCodec, deriveStore, storageMode, withPrefix)

case storageMode of
    Persistent -> deriveStore ''Account
    Ephemeral -> pure []

let options = withPrefix "account" defaultOptions
 in deriveCodec options ''Account

case storageMode of Persistent -> deriveStore ''Ledger; Ephemeral -> pure []
