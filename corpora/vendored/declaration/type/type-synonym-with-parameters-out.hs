module Parser.Types where

type Parser a = String -> Either String (a, String)

type Pair a b = (a, b)

type Handler m = Request -> m Response
