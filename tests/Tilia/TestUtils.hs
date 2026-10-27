-- | Helpers several specs share.
module Tilia.TestUtils
  ( withEnvironment,
    parsed,
    commentsIn,
  )
where

import Control.Exception (bracket)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Tilia.Comments (Comment)
import Tilia.Parser
  ( ParsedModule (..),
    defaultParserConfig,
    describeParseError,
    parseModule,
  )
import Tilia.Source (comments)

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

-- | Parse a module, or stop the test saying why it does not parse.
parsed :: Text -> ParsedModule
parsed source = case parseModule defaultParserConfig "M.hs" source of
  Left problem -> error (T.unpack (describeParseError problem))
  Right m -> m

-- | Every comment in a module.
commentsIn :: Text -> [Comment]
commentsIn = comments . pmSource . parsed
