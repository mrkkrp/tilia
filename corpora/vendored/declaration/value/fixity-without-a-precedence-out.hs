{-# LANGUAGE ExplicitNamespaces #-}

-- A fixity declaration written without a precedence stays without one.
module Rope.Join where

infixr `joinedTo`

infixl 5 `besides`

infix <~>, `against`

infixr type :~

class Splice r where
  infixl `splice`
  splice :: r -> r -> r

data a :~ b = a :~ b

joinedTo, besides, (<~>), against :: String -> String -> String
joinedTo = (++)
besides = (++)
(<~>) = (++)
against = (++)
