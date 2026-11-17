{-# LANGUAGE CPP #-}

module A where

#ifdef WITH_LABELS
class Shelved e where
    label :: e -> String
#else
type Label = ()

class Shelved e where
#endif

    putAway :: e -> IO ()
