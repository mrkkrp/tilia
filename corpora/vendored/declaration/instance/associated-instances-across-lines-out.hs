{-# LANGUAGE TypeFamilies #-}

module Board.Rook where

data Rook = Rook

instance Piece Rook where
  type
    Move
      Rook =
      Slide
  type
    Square Rook =
      (Int, Int)
  newtype
    Capture
      Rook
      side
    = Taken side
  data Promotion Rook
    = Never
  moves _ = []
