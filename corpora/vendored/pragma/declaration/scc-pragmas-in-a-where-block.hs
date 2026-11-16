module Report.Render where

render :: [Int] -> String
render rows = header ++ body ++ footer
  where
    header = "rows:"
    {-# SCC header #-}
    body = concatMap show rows
    {-# SCC body "render_body" #-}
    footer = "."
    {-# SCC footer
          "render_footer" #-}
