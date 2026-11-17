module Toolchain.Link where

link :: [Object] -> IO ()
link objects = do
    { -- The linker cares about the order.
      sorted <- order objects

      -- Libraries go last.
    ; libraries <- resolve sorted
      -- It reads a response file.
    ; run "ld" (sorted <> libraries) -- and nothing else
    }
