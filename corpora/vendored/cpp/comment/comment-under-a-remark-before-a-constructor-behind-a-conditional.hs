{-# LANGUAGE CPP #-}

module Network.Endpoint where

data Endpoint =
  Tcp {
    tcpPort :: Int
  } -- the default
  -- every platform has TCP
#ifdef WITH_UNIX
  | Unix
#endif
