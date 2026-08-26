import Data.List
  (
    -- on a line of its own
    sort,
  )

parenthesised =
  (
    -- on a line of its own
    value
  )

listed =
  [
    -- on a line of its own
    first,
    second
  ]

typed ::
  (
    -- on a line of its own
    Int
  )
typed = 0

data Colour
  = Red
  |
    -- on a line of its own
    Green

data Shape = Circle
  {
    -- on a line of its own
    radius :: Int
  }
