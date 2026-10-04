module Survey.Tally where

-- answers arrive as text and are counted per question
import Survey.Answer (Answer, answerText)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

tally :: [Answer] -> Map String Int
tally = foldr (\a -> Map.insertWith (+) (answerText a) 1) Map.empty
