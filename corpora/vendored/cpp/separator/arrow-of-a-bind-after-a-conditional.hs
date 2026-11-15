{-# LANGUAGE CPP #-}

module Session.Expiry where

expired :: Manager -> IO Status
expired manager = do
#if MIN_VERSION_http_client(0,7,0)
  Left (Rejected _ (Reply status _))
#else
  Left (Rejected status _)
#endif
    <- request manager
  pure status
