afterAnEquals n =
  let step = -- one at a time
        -- and no faster than that
        1
   in n + step

afterAnArrow n =
  case compare n 0 of
    GT -> -- above zero
      -- so we climb
      climb n
    EQ -> stay
    LT -> fall n

afterThen n =
  if n > 0
    then -- the only interesting branch
      -- and the only one worth a remark
      climb n
    else fall n

afterElse n =
  if n > 0
    then climb n
    else -- everything that is left
      -- which is most of it
      fall n
