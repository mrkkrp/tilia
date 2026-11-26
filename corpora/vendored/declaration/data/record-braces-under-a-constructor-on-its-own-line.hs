module Network.Endpoint where

data Endpoint =
  Tcp {
    tcpPort :: Int
  }

data Address
  = Address
  { addressHost :: String,
    addressPort :: Int
  }

newtype Port
  = Port
  { unPort :: Int
  }

data Socket = Socket
  { socketFd :: Int
  }
