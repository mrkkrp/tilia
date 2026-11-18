module Report.Flush where

flush :: Mode -> IO ()
flush mode = do
    case mode of { Plain -> do hFlush stdout }
    putStrLn "done"

render :: Mode -> Report -> IO ()
render mode report =
    let cleanup = case mode of { Plain -> do hFlush stdout }; quiet = True
     in layout report

finish :: Mode -> IO ()
finish mode = do { layout; case mode of { Plain -> do hFlush stdout } }

replay :: IO ()
replay = do { entry <- do { readEntry; do flush }; apply entry }
