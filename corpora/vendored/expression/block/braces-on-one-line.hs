{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}

module Toolchain.Verilator where

program :: String -> String
program os =
  let p = case os of {"mingw32" -> "verilator_bin"; _ -> "verilator"}
   in p

banner :: IO ()
banner = do { putStrLn "verilator"; putStrLn "ready" }

level :: Int -> String
level = \case { 0 -> "quiet"; _ -> "verbose" }

timeout :: Int -> Int
timeout n = let { base = 10; step = 5 } in base + step * n

retries :: Int
retries = attempts where { attempts = 3; delay = 100 }

warnings :: [String]
warnings = let { ?pedantic = True } in collect
