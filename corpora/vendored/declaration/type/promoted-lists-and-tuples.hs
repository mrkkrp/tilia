{-# LANGUAGE DataKinds #-}

-- A promoted list whose first item is promoted itself keeps a space after
-- its tick, or the two ticks would read as a character literal.
module Api.Routes where

type Methods = Proxy '[ 'Get, 'Post]

type Columns = Proxy '[Int,String]

type Header = Proxy '(Int, Bool)

type None = Proxy '[]
