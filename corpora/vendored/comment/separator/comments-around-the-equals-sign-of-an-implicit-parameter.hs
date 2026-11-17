{-# LANGUAGE ImplicitParams #-}

module Report.Render where

render report =
  let ?width = -- of the terminal
        terminalWidth
   in layout report

renderFlushed report = do
  let ?cleanup = -- before exiting
        do
          hFlush stdout
  layout report

renderPadded report = layout report
  where
    ?width -- of the terminal
      = terminalWidth
    ?height {- in lines -} = terminalHeight
