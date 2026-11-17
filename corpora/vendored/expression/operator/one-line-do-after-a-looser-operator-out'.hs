module Config.Lookup where

import Control.Applicative ((<|>))

setting :: Key -> IO (Maybe Text)
setting key =
    fromEnvironment key
        <|> do fromFile key
            <> fromDefaults key

refresh :: Job -> IO Result
refresh job =
    prepare job
        >>= do run job
            <> report job
            <|> skip job
