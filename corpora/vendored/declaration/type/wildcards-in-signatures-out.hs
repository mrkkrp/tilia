{-# LANGUAGE PartialTypeSignatures #-}

module Survey.Answer where

tally :: _ -> Int
tally = length

firstWord :: String -> Maybe _
firstWord = fmap fst . uncons . words

shown :: _ => a -> String
shown = show

parsed = read @_ "12" :: Int
