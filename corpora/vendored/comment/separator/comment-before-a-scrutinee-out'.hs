-- A comment on lines of its own between a case and its scrutinee, or
-- between a minus and what it negates, puts what follows it a step in,
-- where at the column of the case in a do block it would begin a statement.
module Kripke.Explicit where

successors model w = do
    case
        -- the relation is kept by world
        lookup w (relation model) of
        Nothing -> pure []
        Just ws -> pure ws

depth n = do
    level <- current
    -
        -- below the surface
        level
