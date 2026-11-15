{-# LANGUAGE UnboxedSums #-}

module Lexer.Token where

classify :: Char -> (# Int | Char | () #)
classify c
  | isDigit c = (#digitToInt c| | #)
  | isAlpha c = (# |c | #)
  | otherwise = (# | |  () #)

describe :: (# Int | Char | () #) -> String
describe (#n | | #) = show n
describe (# | c| #) = [c]
describe (# | | ()#) = "blank"
