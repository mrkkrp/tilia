tupleOfHalves =
  combine
    ( north,
      south
    ) -- the two halves
    ( east,
      west
    ) -- and the other two

nestedRuns =
  render
    [ [ alpha,
        beta
      ] -- the inner run
    ] -- the outer run

runningTotal =
  weigh
    ( heavier
        + lighter
    ) -- everything so far

blockAfterBracket =
  measure
    ( width,
      height
    ) {- taken at the widest point -}
