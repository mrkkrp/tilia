-- Block comments written between what is defined and its equals sign stay
-- there, however many of them there are.
summarized {- options -} {- tree -} =
  foldr step []

applied x {- before -} {- the sign -} =
  x

oneLine {- first -} {- second -} = 0

several {- one -} {- two -} {- three -} =
  ()
