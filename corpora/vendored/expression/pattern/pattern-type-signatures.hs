module Server.Guard where

import Control.Exception (SomeException, handle)

quietly :: IO () -> IO ()
quietly = handle (\(_ :: SomeException) -> pure ())

widen :: Int -> Integer
widen (n::Int) = fromIntegral n
