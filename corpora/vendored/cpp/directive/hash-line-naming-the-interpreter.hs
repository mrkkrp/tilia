#!/usr/bin/env runghc
{-# LANGUAGE CPP #-}

-- The line naming the interpreter opens with a hash, but with no keyword
-- of the preprocessor after it, so it is no directive.
module Main (main) where

main :: IO ()
#ifdef VERBOSE
main = putStrLn "sweeping the chimney" >> sweep
#else
main = sweep
#endif

sweep :: IO ()
sweep  =  pure ()
