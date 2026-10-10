{-# LANGUAGE CPP #-}

module Interpreter.Run (run) where

run :: [String] -> IO ()
run args =
#ifndef SAFE
  ifNotRunning $ do
#else
  do
#endif
    s <- newSession
    execute s args
