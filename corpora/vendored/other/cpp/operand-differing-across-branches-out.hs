{-# LANGUAGE CPP #-}

module Main (main) where

main :: IO ()
main = do
  when (verbosity >= 4)
#if MIN_VERSION_base(4,10,0)
    $
      let dump = putStrLn . render
            where
              render = show
       in liftIO $ dump settings
#elif MIN_VERSION_base(4,9,0)
    $
      let dump = print
       in liftIO $ dump settings
#else
    $ liftIO
    $ dump settings
#endif

  when (verbosity >= 3) $ do
    putStrLn "done"
