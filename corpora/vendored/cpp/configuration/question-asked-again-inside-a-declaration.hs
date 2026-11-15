{-# LANGUAGE CPP #-}

module Network.Client (connect) where

#if MIN_VERSION_network(3,0,0)
connect :: Socket -> SockAddr -> Int -> IO Connection
connect sock addr timeout = do
#else
connect :: Socket -> SockAddr -> IO Connection
connect sock addr = do
#endif
  handle <- socketToHandle sock ReadWriteMode
  peer <- getPeerName sock
#if MIN_VERSION_network(3,0,0)
  withTimeout timeout (open handle peer)
#else
  open handle peer
#endif
