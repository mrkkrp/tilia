module Parser.Sequence where

lexeme =
    do { skipSpace }
    >>= \_ -> do
        token <- word
        pure token
