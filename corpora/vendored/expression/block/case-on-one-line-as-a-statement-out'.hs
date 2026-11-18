module Report.Flush where

flush :: Mode -> IO ()
flush mode = do
    case mode of Plain -> hFlush stdout
    putStrLn "done"

flushLast :: Mode -> IO ()
flushLast mode = do
    putStrLn "done"
    case mode of Plain -> do hFlush stdout

flushTyped :: Mode -> IO ()
flushTyped mode =
    do
        case mode of Plain -> hFlush stdout
        putStrLn "done"
    :: IO ()

flushOrSkip :: Mode -> IO ()
flushOrSkip mode = do
    putStrLn "done"
    do { layout; case mode of Plain -> do hFlush stdout }
        <|> pure ()

finish :: Mode -> IO ()
finish mode = do { layout; case mode of Plain -> hFlush stdout }
