infixl 2 `stackOn`

infix 9 `keyedBy`

infixr 1 `orElse`

onOneLine = base `stackOn` middle `keyedBy` name

tighterOperatorNestsUnderLooser = base
  `stackOn` middle
  `keyedBy` name

behindADollar = render $ base
  `stackOn` middle
  `keyedBy` name

oneLevelStaysFlat = first `orElse` second `orElse` third

oneLevelStaysFlatWhenSpread = first
  `orElse` second
  `orElse` third
