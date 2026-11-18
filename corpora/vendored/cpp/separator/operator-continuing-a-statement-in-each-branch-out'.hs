{-# LANGUAGE CPP #-}

module Report (report, flush) where

report :: Double -> IO ()
report total = do
    print
#ifdef ROUNDED
        $
            round total
#else
        $
            truncate total
#endif
    hFlush stdout

flush :: [String] -> IO ()
flush items = do
    forM_ items
#ifdef VERBOSE
        $ \item -> print item $ do
#else
        $ \item -> putStrLn item $ do
#endif
        hFlush stdout
    pure ()
