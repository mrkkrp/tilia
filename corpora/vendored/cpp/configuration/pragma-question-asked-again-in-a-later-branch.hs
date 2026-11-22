{-# LANGUAGE CPP #-}
#ifdef HAVE_QUOTES
{-# LANGUAGE TemplateHaskellQuotes #-}
#endif
module Lantern.Name (lanternName) where

import Language.Haskell.TH (Name, mkName)

data Lantern = Lantern

lanternName :: Name
#ifdef BY_STRING
lanternName = mkName "Lantern"
#else
#ifdef HAVE_QUOTES
lanternName = 'Lantern
#else
lanternName = mkName "Lantern.Lantern"
#endif
#endif
