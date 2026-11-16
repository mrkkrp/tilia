module Text.Join where

joinText :: Text -> Text -> Text
joinText = (<>) @Text

prependAll :: [Int] -> [[Int]] -> [[Int]]
prependAll = zipWith ((:) @Int)
