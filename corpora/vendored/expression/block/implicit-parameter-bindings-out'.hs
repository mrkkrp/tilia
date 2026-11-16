{-# LANGUAGE ImplicitParams #-}

module Report.Render where

render :: Report -> String
render report =
    let ?margins = (2, 4)
        ?columns = [Title, Total]
     in layout report

renderWith :: Mode -> Report -> String
renderWith mode report =
    let ?style = case mode of { Plain -> plainStyle; Fancy -> fancyStyle }
     in layout report
