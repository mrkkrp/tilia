-- A comment under the remark that ends the line above, lined up with it or
-- indented further, carries the remark on, even where the code under it
-- begins at the same column.
data AttrTable a = -- for all attribute identifiers not contained in the
                   -- finite map the value is 'undef'
                   SoftTable a  -- updated attributes
                 | FrozenTable a

data Part
  = Literal String  -- a literal string
  | Variable String  -- some input; the
                          -- string names the variable
  | Optional Part

decode :: String -> Either String String
decode b = -- returning an Either means the whole result
           -- is in memory at once anyway
           case reverse b of
             [] -> Left "empty"
             s -> Right s
