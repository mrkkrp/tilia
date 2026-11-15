{-# LANGUAGE OverloadedStrings #-}

-- Copyright the survey's authors. Released under the BSD3 licence.

import System.IO (stdout)
import Data.Text (Text)
import qualified Data.Text.IO as Text

main :: IO ()
main = Text.hPutStrLn stdout ("questions" :: Text)
