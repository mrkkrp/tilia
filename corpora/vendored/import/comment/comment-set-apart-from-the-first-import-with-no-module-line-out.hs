{-# LANGUAGE OverloadedStrings #-}

-- Copyright the survey's authors. Released under the BSD3 licence.

import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.IO (stdout)

main :: IO ()
main = Text.hPutStrLn stdout ("questions" :: Text)
