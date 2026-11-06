{-# LANGUAGE CPP #-}

module Main (main) where

main :: IO ()
main = do
  putStrLn "start"
  -- Sockets are not available on Windows.

#if !defined(mingw32_HOST_OS)
  putStrLn "sockets"
#endif
  putStrLn "stop"
