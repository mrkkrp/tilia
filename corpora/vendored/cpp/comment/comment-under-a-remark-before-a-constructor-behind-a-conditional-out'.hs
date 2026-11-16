{-# LANGUAGE CPP #-}

module Network.Endpoint where

data Endpoint
    =
#ifdef WITH_UNIX
    Tcp
        { tcpPort :: Int
        } -- the default
    | -- every platform has TCP
      Unix
#else
    Tcp
    { tcpPort :: Int
    } -- the default
-- every platform has TCP
#endif
