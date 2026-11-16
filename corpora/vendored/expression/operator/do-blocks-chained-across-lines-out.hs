module Quiz.Bonus where

import Control.Applicative ((<|>))

bonuses :: [Int]
bonuses =
  do
    let base = 10
    base * 2
    : do
      5
    : []

sign :: Parser (Int -> Int)
sign =
  do
    char '-'
    pure negate
    <|> do
      char '+'
      pure id
    <|> pure id
