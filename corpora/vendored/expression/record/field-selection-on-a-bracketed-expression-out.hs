{-# LANGUAGE OverloadedRecordDot #-}

module Staff.Lookup where

deskOf :: Directory -> String -> Desk
deskOf directory key = (find key directory).desk

firstLevel :: [Person] -> Int
firstLevel people = (head people).desk.level
