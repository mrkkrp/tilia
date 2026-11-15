{-# LANGUAGE TemplateHaskell #-}

module Query.Column where

import Language.Haskell.TH (conT)

count :: $(conT ''Int)
count = 0

label :: Maybe $columnType -> String
label = maybe "" show
