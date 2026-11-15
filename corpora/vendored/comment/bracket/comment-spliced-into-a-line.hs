module Terminal.Size where

resize d = case d of
  Size{ rows = r, columns = c
      {-, depth = d-} } -> r + c
