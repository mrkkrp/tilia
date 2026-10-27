-- | Helpers several specs share.
module Tilia.TestUtils
  ( withEnvironment,
  )
where

import Control.Exception (bracket)
import Data.Foldable (traverse_)
import System.Environment (lookupEnv, setEnv, unsetEnv)

-- | Run something with the given variables set, and the environment as it
-- was afterwards however it turns out.
withEnvironment :: [(String, String)] -> IO a -> IO a
withEnvironment vars act = bracket set restore (const act)
  where
    set = traverse remember vars
    remember (key, value) = do
      was <- lookupEnv key
      setEnv key value
      pure (key, was)
    restore = traverse_ (\(key, was) -> maybe (unsetEnv key) (setEnv key) was)
