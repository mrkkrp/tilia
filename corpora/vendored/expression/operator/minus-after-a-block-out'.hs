{-# LANGUAGE LambdaCase #-}

-- A minus on a line of its own after a block puts the block's items a step
-- further in, as an argument after a block does, where at their column it
-- would begin another item and negate it.
module Class.Indent where

column inst =
    let width = case inst of
                Instance binds -> length binds
                _ -> 0
            - 1
     in width

depth = do
        level <- current
        pure level
    - 1

margin =
    2 * \case
            Wide -> 8
            Narrow -> 4
        - 1
