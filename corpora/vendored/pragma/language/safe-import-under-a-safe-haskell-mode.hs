{-# LANGUAGE Haskell98 #-}
{-# LANGUAGE Trustworthy #-}
module Kiln.Glaze (glazes) where

-- Haskell98 has no foreign imports, so safe is a keyword only because
-- Trustworthy puts a Safe Haskell mode in force.
import safe Data.List (sort)

glazes :: [String] -> [String]
glazes = sort
