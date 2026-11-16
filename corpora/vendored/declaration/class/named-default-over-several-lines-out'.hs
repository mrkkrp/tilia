{-# LANGUAGE NamedDefaults #-}

-- The class a named default is for stays on the line of default.
module Calc.Literal where

import Data.String (IsString)
import Data.Text (Text)

default IsString
    ( Text
    , String
    )
