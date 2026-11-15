everyOperandAnnotated =
  base -- what we started from
    + delta -- what was added
    - refund -- and what came back

annotatedOperators =
  base
    + delta -- the adjustment
    - refund -- the reimbursement

remarkBetweenOperatorAndOperand =
  opening
    <> -- said once
    -- and never twice
    closing

lastOperandOnly =
  first
    `mappend` second
    `mappend` third -- and no further
