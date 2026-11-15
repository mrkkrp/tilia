{-# LANGUAGE DataKinds #-}

module Door.State where

data Lock = Locked | Open

newtype Door (s :: Lock) = Door String

shut :: Door 'Open -> Door 'Locked
shut (Door name) = Door name

type Always = Proxy 'True
