{-# LANGUAGE CPP #-}

-- Bindings lined up after a let leave a conditional that goes on
-- past them in two.
module Data.Witness where

trace :: Witness a -> IO ()
trace w = do
  let
    name = "witness"
#if WITNESS_DEBUG
    shown = show (Debugged w)
#if __GLASGOW_HASKELL__ >= 810
  putStrLn shown
#endif
#endif
  putStrLn name
