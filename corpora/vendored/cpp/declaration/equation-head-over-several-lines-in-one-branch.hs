{-# LANGUAGE CPP #-}

module Network.Client (connect, pick) where

-- | Opens a connection over a socket.
#if MIN_VERSION_network(3,0,0)
connect :: Socket -> SockAddr -> Int -> IO Connection
connect sock addr
        timeout = do
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

-- | Picks a port, unless one is given.
pick :: Int -> Maybe Int -> Int
pick n m = case m of
#if MIN_VERSION_base(4,10,0)
  Just
    k -> do
#else
  Just k -> do
#endif
    let r = k + n
    r
  Nothing -> n
