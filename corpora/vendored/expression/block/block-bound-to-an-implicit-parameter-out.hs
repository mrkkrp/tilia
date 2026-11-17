{-# LANGUAGE ImplicitParams #-}

module Report.Render where

render :: Report -> IO ()
render report =
  let ?cleanup = do
        hFlush stdout
        hFlush stderr
   in layout report

renderIn :: Mode -> Report -> String
renderIn mode report =
  let ?style = case mode of
        Plain -> plainStyle
        Fancy -> fancyStyle
      ?margins =
        case mode of
          Plain -> (0, 0)
          Fancy -> (2, 4)
   in layout report
