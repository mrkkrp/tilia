module Org.Chart where

import qualified Data.Map as Map

infix 6 =:

(=:) :: String -> String -> (String, String)
k =: v = (k, v)

chain :: Map.Map Int Int -> Int -> [Int]
chain bosses n =
    n : case Map.lookup n bosses of
        Nothing -> []
        Just b -> chain bosses b

chain' :: Map.Map Int Int -> Int -> [Int]
chain' bosses n =
    n
        : case Map.lookup n bosses of
            Nothing -> []
            Just b -> chain' bosses b

card :: Person -> [(String, String)]
card p =
    reverse $
        "name" =: personName p
            : "team" =: personTeam p
            : case personPhone p of
                Nothing -> []
                Just phone -> ["phone" =: phone]

final :: Char -> String
final c = case c of { '.' -> "stop"; _ -> "go" } : []
