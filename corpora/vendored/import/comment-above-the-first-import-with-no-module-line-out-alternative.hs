{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text.IO as Text
-- the report is written to standard output, one question per line
import System.IO (stdout)

main :: IO ()
main = Text.hPutStrLn stdout ("questions" :: Text)
