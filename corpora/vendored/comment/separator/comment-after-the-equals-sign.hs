module M where

data Answer = {- in the order asked -}
  Yes | No

data Size = {- small
  ones first -}
  Small | Large

newtype Count = {- of answers -}
  Count Int
