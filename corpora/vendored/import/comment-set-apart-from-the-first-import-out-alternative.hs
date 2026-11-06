module Survey.Export where

--------------------------------------------------------------------------------
-- the answers go out as CSV; nothing here touches the database

import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Survey.Answer (Answer, answerText)

row :: [Answer] -> String
row = intercalate "," . fmap answerText
