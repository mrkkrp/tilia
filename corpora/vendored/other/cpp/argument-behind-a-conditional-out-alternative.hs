{-# LANGUAGE CPP #-}

module Records.Process (process) where

import Language.Haskell.TH

process :: [Stmt] -> Exp -> Exp
process statements result =
  DoE
#if MIN_VERSION_template_haskell(2,17,0)
    Nothing
#endif
    (statements ++ [NoBindS result])
