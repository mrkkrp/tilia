-- A comprehension across lines that stands as a statement of a block closes
-- a step in, where its bracket cannot begin another statement.
module Chess.Moves where

moves :: Board -> [Move]
moves board = do
  piece <- pieces board
  [ Move piece square
    | square <- reachable board piece,
      not (occupied board square)
    ]
