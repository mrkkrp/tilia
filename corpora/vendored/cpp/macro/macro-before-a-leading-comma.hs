{-# LANGUAGE CPP #-}

module Signals (handled) where

#define ALSO(name) , name

handled :: [String]
handled =
  [ "INT"
  , "TERM"
  ALSO("HUP")
  , "QUIT"
  ]
