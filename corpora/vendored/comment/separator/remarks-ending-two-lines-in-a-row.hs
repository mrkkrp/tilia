retry x =
  if x then -- once more
    do -- quietly
       attempt x
  else -- for good
    case x of -- whatever it was
      _ -> giveUp
