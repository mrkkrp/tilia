-- A fixity declared among local bindings decides the layout of the operator
-- they bind.
module Setting.Fallback where

import Text.Read (readMaybe)

data Setting = Number Int | Switch Bool | Word String

parsed :: String -> Maybe Setting
parsed input =
  Number
    <$> readMaybe input
    <!> Switch
      <$> readMaybe input
    <!> pure (Word input)
  where
    infixl 3 <!>
    Nothing <!> y = y
    x <!> _ = x

firstOf :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
firstOf a b c =
  let infixr 3 <!>
      Nothing <!> y = y
      x <!> _ = x
   in negate
        <$> a
        <!> b
        <!> c
