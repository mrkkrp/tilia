-- A block comment closes itself, so it is printed where the region that
-- carries it is printed. It must not be carried back across the token that
-- opens a body, or it comes out in front of what is being defined.
plain =
  {- one at a time -}
  1

withArguments n m =
  {- takes two -}
  n + m

inAnAlternative n =
  case n of
    0 ->
      {- the base case -}
      stop
    _ -> go n

-- Crossing a token in the middle of a construct is another matter: it
-- neither leaves the construct nor lands in front of it.
inTheMiddle n =
  case n {- worth a look -} of
    0 -> stop
    _ -> go n

-- A line comment owns the rest of its line wherever it is put, so it stays
-- at the end of the line the author wrote it on.
heldBack n = -- one at a time
  n
