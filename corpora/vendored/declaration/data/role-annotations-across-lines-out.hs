{-# LANGUAGE RoleAnnotations #-}

module Cache.Key where

type role
  Key
    nominal
    phantom

data Key k v = Key Int

type role
  Slot
    representational
    _

newtype Slot a b = Slot a

type role
  Entry
    nominal
    representational

data Entry k v = Entry k v
