{-# LANGUAGE CPP #-}

module Garden.Alarm where

frost = do
#ifdef SIREN
    putStrLn "frost tonight"
#endif
