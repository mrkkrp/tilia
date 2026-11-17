{-# LANGUAGE ImplicitParams #-}

module Report.Render where

render :: Report -> IO ()
render report =
  let ?cleanup = do hFlush stdout
      ?style = case mode report of Plain -> plainStyle
   in layout report

renderFlushed :: Report -> IO ()
renderFlushed report = do
  let ?style = case mode report of Plain -> plainStyle
  layout report

renderPadded :: Report -> IO ()
renderPadded report = layout report
  where
    ?style = case mode report of Plain -> plainStyle

renderQuietly :: Mode -> Report -> IO ()
renderQuietly mode report =
  let ?cleanup = case mode of { Plain -> do hFlush stdout }; ?quiet = True
   in layout report
