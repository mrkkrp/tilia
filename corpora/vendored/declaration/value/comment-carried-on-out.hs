-- A comment lined up under a line that ended in one, with nothing below it
-- at that column, carries that remark on rather than beginning one about
-- what follows.
carriedOn xs ys =
  [ (a, b)
  | a <-
      xs
        + xs -- said once
        -- and never twice
  | b <- ys
  ]

-- The same, where what follows is not a sibling but nothing at all.
class
  a -- said once
    :+ b -- said twice
    -- and a third time

-- Code below at the comment's own column is code the comment sits over,
-- however the line above ended.
sitsOver =
  opening
    <> -- said once
    -- and never twice
    closing

-- Lined up with nothing, because the line above was blank.
afterAGap = do
  a --

  bar

-- Lined up with a line that has a comment on it but does not end in one, so
-- there is no remark above to carry on.
codeAfterTheComment =
  g
    (a {- said once -} + b)
  -- and this is about something else
  where
    h = 1
