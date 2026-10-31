module Survey.Tally
  ( count,
    {- the total -} -- so far
    reset,
  )
where

import Data.List
  ( {- for the order -} -- of answers
    sortOn,
  )

count =
  {- every answer -} -- once
  length answers

data Answer
  = {- in the order asked -} -- on the form
    Yes
  | No

verdict a = case a of
  Yes ->
    {- counted -} -- as one
    1
  No -> 0

skipped = \a ->
  {- left out -} -- of the count
  0
