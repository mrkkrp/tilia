module Report.Pretty where

class () => Pretty a where
  pretty :: a -> String

class (Show a) => Loud a where
  loud :: a -> String

width :: () => Int
width = 80
