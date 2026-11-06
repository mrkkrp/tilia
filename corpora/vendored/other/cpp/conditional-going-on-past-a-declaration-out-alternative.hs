{-# LANGUAGE CPP #-}

module Socket.Spec where

-- | Everything the server answers.
spec :: Spec
spec = do
  describe "routes" $ do
    it "serves GET" $ get "/" `shouldRespondWith` 200
    it "serves POST" $ post "/" "" `shouldRespondWith` 200 -- and nothing else
#if !defined(mingw32_HOST_OS)
  -- No unix sockets on Windows.
  describe "sockets"
    $ it "works with a unix socket"
    $ withServer socketPath
  where
    socketPath = "/tmp/test.socket"

-- | Run the server on a socket for as long as the inner action runs.
withServer :: FilePath -> IO a -> IO a
withServer path inner = bracket (listenOn path) close (const inner)
#endif
