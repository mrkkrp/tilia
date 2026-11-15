module Survey.Merge where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
{- Answers from older surveys are migrated before they are merged. -}
-- The migration is kept apart from the merge itself.
import Survey.Migrate (migrate)

merge :: [(Int, String)] -> [(Int, String)]
merge = sortOn fst . Map.toList . Map.fromList . fmap migrate
