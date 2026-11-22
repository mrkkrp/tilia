-- A comment lined up with a line inside a list or a comprehension that
-- follows an operator lines up with its brackets.
module Pottery.Glaze where

oxides :: [String]
oxides =
    ["silica"] ++
    [ "alumina"
        , "flux" ]
        -- in that order

colourants :: [String]
colourants =
    ["iron"] <>
    [c
        | c <- ["cobalt", "copper"]]
        -- by the gram

opacifiers :: [String]
opacifiers =
    [ "tin"
        , "zircon" ]
        -- and nothing else
