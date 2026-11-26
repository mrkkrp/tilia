module Kiln.Config where

quiet config = config -- before the brace
  { verbose = False }

heat = fire -- before the parenthesis
  ( low, high )

stack = pile -- before the bracket
  [ low, high ]
