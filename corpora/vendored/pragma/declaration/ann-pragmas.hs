module Report.Lint where

{-# ANN module "HLint: ignore Use camelCase" #-}

{-# ANN type Report_Row "HLint: ignore" #-}
data Report_Row = Report_Row

{-# ANN render_row "HLint: ignore Eta reduce" #-}
render_row :: Report_Row -> String
render_row r = describe r
