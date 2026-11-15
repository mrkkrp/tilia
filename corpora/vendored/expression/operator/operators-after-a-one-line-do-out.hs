module Parser.Alternatives (notFollowedBy, option, choice, keyword, signed, escape) where

import Control.Applicative ((<|>))

notFollowedBy p =
  try
    ( do c <- try p; unexpected (show c)
        <|> return ()
    )

option x p =
  do r <- p; pure r
    <|> pure x

choice p q = do a <- p; pure a
  <|> q

keyword = do
  w <- do string "let"; pure Let
    <|> do string "in"; pure In
  pure w

signed p =
  negate <$> do char '-'; p
    <|> p

escape = do
  char '\\'
  do gap; pure Nothing
    <|> do c <- code; pure (Just c)
