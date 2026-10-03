{-# LANGUAGE PatternSynonyms #-}

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

-- An operator written at the end of a line goes to the start of the next,
-- and a comment written after it stays with the operand it follows.
operatorsAtLineEnds =
  base -- what we started from
    + delta -- what was added
    - refund -- and what came back

-- The same, one level of precedence down.
tighterOperatorAtLineEnd =
  base
    + rate -- per unit
      * units

-- The same, for a constructor in a pattern.
headOf
  ( x -- the one we want
      :| _
    ) = x

-- The same, for a pattern synonym.
pattern x -- the head
  :< xs =
  (x, xs)
