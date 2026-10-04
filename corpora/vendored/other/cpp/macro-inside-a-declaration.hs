{-# LANGUAGE CPP #-}

module Session.Log where

#define LOG(s) trace (s) (pure ())
#define STRICT(x) {-# UNPACK #-} !(x)

data Pair = Pair Int
  STRICT(Int)
  deriving (Eq)

open :: IO ()
open = do
  connect
  LOG("opened")
  where
    connect = pure ()

instance Session IO where
  close = do
    disconnect
    LOG("closed")
    where
      disconnect = pure ()
