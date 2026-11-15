{-# LANGUAGE OverloadedStrings #-}

-- the report is written to standard output, one question per line
import System.IO (stdout)
import Data.Text (Text)
import qualified Data.Text.IO as Text

main :: IO ()
main = Text.hPutStrLn stdout ("questions" :: Text)
