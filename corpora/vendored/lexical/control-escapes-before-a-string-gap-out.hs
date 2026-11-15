-- A control escape ending in a backslash is not taken for the start of a
-- string gap that follows it.
module Terminal.Record where

record :: String
record =
  "name\^\\
  \age\^\\
  \town"

bold :: String
bold =
  "\^[[1m\
  \done"
