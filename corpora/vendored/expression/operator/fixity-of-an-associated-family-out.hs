{-# LANGUAGE TypeFamilies #-}

-- A fixity declared for a family a class associates applies among types
-- only: ++ binds tighter than <> in Bundle, and looser in bundle, where
-- both are the Prelude's.
module Crate.Bundle where

infixl 7 ++
infixl 6 <>

class Packable a where
  type a ++ b
  type a <> b
  packWith :: a -> b -> a ++ b

type Bundle a b c =
  a
    <> b
      ++ c

bundle :: String -> String -> String -> String
bundle a b c =
  a
    ++ b
      <> c
