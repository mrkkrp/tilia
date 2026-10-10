{-# LANGUAGE CPP #-}

module Interpreter.Run (run) where

run :: [String] -> IO ()
run args =
#ifndef SAFE
  ifNotRunning $
#endif
    do s <- newSession
       execute s args
