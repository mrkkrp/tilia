-- Where a remark ends the line of an operator and another that of its
-- operand, the operand stays on its line, and what carries it on with it.
guess =
  catA
    [ isBinaryDoc
        >>> -- 0. guess
        constA isoLatin1, -- the content
                          -- this handling
      getChildren -- 1. guess
    ]
