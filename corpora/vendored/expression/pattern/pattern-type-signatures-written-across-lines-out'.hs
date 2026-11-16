module Config.Load where

load :: IO Config
load = do
    port :: Int <- readSetting "port"
    (host, path)
        :: (String, FilePath) <-
        readLocation
    pure (Config host port path)
