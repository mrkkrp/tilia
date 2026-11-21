retry x =
  if x
    then -- once more
      do -- quietly
        attempt x
    else -- for good
      case x of -- whatever it was
        _ -> giveUp

keywords =
  [ "case",
    "where"
  ]
    ++ -- extensions
    [ "mdo", -- RecursiveDo
      "rec" -- Arrows
    ]

record =
  string "ATOM"
    <|> -- or
    string "HETATM" -- with a heteroatom
    <|> string "TER"
