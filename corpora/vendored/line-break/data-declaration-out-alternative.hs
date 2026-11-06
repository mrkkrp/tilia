-- A data declaration that runs past column 80 is broken before its
-- constructors, one per line, and before its deriving clause.
module Loom.Data where

data Fibre = Cotton | Wool | Silk | Linen | Hemp | Jute | Ramie | Bamboo | Alpaca
  deriving (Eq, Show)
