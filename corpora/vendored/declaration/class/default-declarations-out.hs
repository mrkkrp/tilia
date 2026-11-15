{-# LANGUAGE NamedDefaults #-}

module Calc.Literal where

import Data.String (IsString)
import Data.Text (Text)

default (Integer, Double)

default IsString (Text)
