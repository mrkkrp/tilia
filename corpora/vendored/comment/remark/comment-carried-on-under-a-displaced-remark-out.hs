-- Where an operator joins its operand, their remarks are for one line. The
-- second goes on a line of its own, and what carries it on goes with it.
guess =
  catA
    [ isBinaryDoc
        >>> constA isoLatin1, -- 0. guess
        -- the content
        -- this handling
      getChildren -- 1. guess
    ]
