module Toolchain.Build where

build :: Target -> IO ()
build target = do { sources <- collect target
                  ; objects <- mapM compile sources

                  ; link objects }

flags :: Mode -> [String]
flags mode = case mode of {
  Debug -> ["-g"];
  Release -> ["-O2"]
}

compile :: Source -> IO Object
compile source = case language source of
  { C -> do
      run "cc" [path source]
      object source
  ; Haskell -> run "ghc" [path source] >> object source
  }

suffix :: Platform -> String
suffix platform = extension platform
  where { extension Windows = ".exe"
        ; extension _ = "" }

jobs :: Int
jobs = let { cores = 8
           ; spare = 1 } in cores - spare
