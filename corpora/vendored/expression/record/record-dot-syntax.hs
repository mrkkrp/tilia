{-# LANGUAGE OverloadedRecordDot #-}

module Staff.Directory where

data Person = Person {name :: String, desk :: Desk}

newtype Desk = Desk {level :: Int}

nameOf :: Person -> String
nameOf p = p.name

levels :: [Person] -> [Int]
levels = map (.desk.level)

label :: Person -> String
label p = p.name <> " on level " <> show p.desk.level
