{-# LANGUAGE NamedDefaults #-}

module Calc.Text
  ( default IsString,
    Text,
    pack,
  )
where

import Data.String (IsString)
import Data.Text (Text, pack)

default IsString (Text)
