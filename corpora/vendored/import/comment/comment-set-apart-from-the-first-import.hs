module Survey.Export where

--------------------------------------------------------------------------------
-- the answers go out as CSV; nothing here touches the database

import Survey.Answer (Answer, answerText)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map

row :: [Answer] -> String
row = intercalate "," . fmap answerText
