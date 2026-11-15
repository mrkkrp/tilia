{-# LANGUAGE DataKinds #-}

module Api.Paths where

type Users = Proxy "users"

type Limit = Proxy 100

type Initial = Proxy 'u'
