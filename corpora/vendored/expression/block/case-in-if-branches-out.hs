module Parcel.Label where

label :: Bool -> Maybe String -> String
label express recipient =
  if express
    then case recipient of
      Just r -> "URGENT: " ++ r
      Nothing -> "URGENT"
    else case recipient of
      Just r -> r
      Nothing -> "unaddressed"
