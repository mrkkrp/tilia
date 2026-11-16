module Job.Runner where

enqueue :: Queue -> IO ()
enqueue q = retrying (do push q; flush q) 3

budget :: Int
budget =
    ( let base = 40
       in base + 2
    )

bounds :: (Int, Int)
bounds =
    ( let lo = 1
       in lo
    , let hi = 99
       in hi
    )

prompt :: IO ()
prompt = do
    ( putStr "> "
        >> hFlush stdout
        )
    line <- getLine
    run line
