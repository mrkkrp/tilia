-- Where an operator joins its operand, their remarks are for one line. The
-- second goes on a line of its own, and what carries it on goes with it.
guess = catA [ isBinaryDoc
               >>>                    -- 0. guess
               constA isoLatin1       -- the content
                                      -- this handling
             , getChildren            -- 1. guess
             ]
