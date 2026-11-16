{-# LANGUAGE TypeFamilies #-}

module Board.Piece where

import Data.Kind (Type)

class Piece p where
  type Move
    p
  type Square p
    :: Type
  data Capture
    p side
  moves :: p -> [Move p]

class Board b where type Cell b
                         :: Type
