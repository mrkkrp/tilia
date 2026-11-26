module Network.Endpoint where

data Endpoint
    = Tcp
        { tcpHost :: String
        , tcpPort :: Int
        }
    | Unix
        { unixPath :: FilePath
        }

data Shape
    = Circle
        { radius :: Double
        }
    | Rect
        { width :: Double
        , height :: Double
        }

data Event
    = Click
        { clickX :: Int
        , clickY :: Int
        }
    | Key
        { keyCode :: Int
        }
    | Quit

data Route
    = Direct
    | Relayed
        { relayHost :: String
        , relayHops :: Int
        }
